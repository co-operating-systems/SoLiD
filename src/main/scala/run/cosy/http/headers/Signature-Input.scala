package run.cosy.http.headers

import akka.http.scaladsl.model.{HttpHeader, ParsingException, Uri}
import akka.http.scaladsl.model.headers.{CustomHeader, RawHeader}
import cats.parse.Parser
import com.nimbusds.jose.util.Base64
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion, HTTPHeaderParseException}

import java.nio.charset.StandardCharsets
import java.security.{PrivateKey, PublicKey, Signature}
import java.time.Instant
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}
import run.cosy.http.headers.Rfc8941
import Rfc8941.{IList, Item, PItem, Parameterized, SfDict, SfInt, SfList, SfString, Token}
import run.cosy.http.Encoding.UnicodeString
import run.cosy.http.headers.SigInput.{hs2019, keyId}

import scala.collection.immutable


/**
 *  [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-02#section-4.1 4.1 The 'Signature-Input' HTTP header]] defined in "Signing HTTP Messages" HttpBis RFC.
 *  since the only algorithm now is "hs2019" we don't allow anything else to be set
 * @param text
 */
final case class `Signature-Input`(sig: SigInputs) extends BetterCustomHeader[`Signature-Input`]:
	override def renderInRequests = true
	override def renderInResponses = true
	override val companion = `Signature-Input`
	override def value: String =
		import Rfc8941.Serialise.given
		sig.si.map{(tk,si)=> (tk,si.il)}.asInstanceOf[Rfc8941.SfDict].canon



object `Signature-Input` extends BetterCustomHeaderCompanion[`Signature-Input`]:
	override val name = "Signature-Input"

	def parse(value: String): Try[SigInputs] =
		Rfc8941.Parser.sfDictionary.parseAll(value) match
			case Left(e) => Failure(HTTPHeaderParseException(e,value))
			case Right(lm) => Success(SigInputs.filterValid(lm))

	def unapply(h: HttpHeader): Option[SigInputs] =
		h match
		case _: (RawHeader | CustomHeader) if h.lowercaseName == lowercaseName => parse(h.value).toOption
		case _ => None

end `Signature-Input`

/**
 * SigInputs are Maps from Signature Names to SigInput entries that this
 * server understands.
 *
 * @param value
 * @return
 */
final case class SigInputs private(val si: ListMap[Rfc8941.Token,SigInput]) extends AnyVal {

	def get(key: Rfc8941.Token): Option[SigInput] = si.get(key)
}

object SigInputs {
	/**
	 * Filter out the inputs that this framework does not accept.
	 * Since this may change with implementations, this should really
	 * be a function provided in a trait provided by the library doing the
	 * verification, so that it can evolve independently of the Http Header
	 * framework.
	 * Todo: make this independent as described above! (when it functions)
	 **/
	def filterValid(lm: SfDict): SigInputs = SigInputs(lm.collect{
		case (sigName, SigInput(sigInput)) => (sigName,sigInput)
	})

}

/**
 * A SigInput is a valid Signature-Input build on an Rfc8941 Internal List.
 * restricted to those this server can understand.
 * todo: An improved version would be more lenient, allowing opt-in refinements.
 *
 * As a Validated data structure, we can keep all the data present in a header for a particular
 * signature, as that is needed to verify the signature itself. Indeed extra attributes will be
 * vital to verify a signatue, since the data from this header is part of the signature
 * @param il
 */
final case class SigInput private(val il: IList) extends AnyVal {
	import Rfc8941.Serialise._

	def headers: Seq[String] = il.items.map{ case PItem(SfString(str),_) => str}

	def keyid: Uri =
		import SigInput.urlStrRegex
		val SfString(urlStrRegex(key))  = il.params(keyId)
		Uri(key)

	def algo = hs2019
	def created: Option[Long] = il.params.get(Token("created")).collect{case SfInt(time) => time}
	def expires: Option[Long] = il.params.get(Token("expires")).collect{case SfInt(time) => time}

}

object SigInput {
	val urlStrRegex = "<(.*)>".r
	val keyId = Token("keyid")
	val acceptedHeadersMap: Map[String, HeaderName] = HeaderName.values.toSeq.map(hn => (hn.toString, hn)).toMap
	val hs2019 = SfString("hs2019")
	val Empty = ListMap.empty[Token, Item]

	def apply(il: IList): Option[SigInput] = if valid(il) then Some(new SigInput(il)) else None

	//this is really functioning as a constructor in pattern matching contexts
	def unapply(pzd: Parameterized): Option[SigInput] =
		pzd match
			case il: IList if valid(il) => Some(new SigInput(il))
			case _                      => None


	/**
	 * A Valid SigInpu IList has a Interal list of parameterless Strings (We don't support those just yet)
	 * and the list has attributes including a keyid, and a protocol. Can have more.
	 * */
	def valid(il: IList): Boolean =
		val headersOk = il.items.forall { itm =>
			itm match
				//we don't support attribute selectors on headers yet
				case PItem(SfString(hdr), Empty) if acceptedHeadersMap.contains(hdr) => true
				case _ => false
		}
		//we need "alg" and "keyid".
		//todo: The restriction on "<" and ">" should really be left to calling code.
		val (keyIdExists, algoExists) = il.params.foldRight((false, false)) { case (param, (keyIdE, algoE)) =>
			param match
				case (Token("keyid"), SfString(id)) if id.startsWith("<") && id.endsWith(">") => (true, algoE)
				case (Token("alg"), SfString("hs2019")) => (keyIdE, true)
				case _ => (keyIdE, algoE)
		}
		headersOk && keyIdExists && algoExists
	end valid
	
}

/**
 * todo: implement header selectors
 * Headers can have attributes used to determine
 *  + [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-2.2 Disctionary Structured Field Members]]
 *  + [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-2.3 List Prefixes]]
 * @param header to select
 * @param selectors ordered list of attributes to select from that header. Order is very important here.
 * todo: it may be better to have HeaderSlectors that each work with a HttpHeader                  
 */
case class HeaderSelector(header: HeaderName, selectors: List[Selector] = List())
sealed trait Selector
case class KeySelector(key: String) extends Selector
case class PrefixSelector(first: SfInt) extends Selector

/**
 * Signature Headers supported.
 * Feel free to add new ones by submitting PRs
 *
 * todo: is this pushing typesafety too far?
 **/
enum HeaderName(val special: Boolean = false):
	case `@request-target` extends HeaderName(true)
	case `@signature-params` extends HeaderName(true)
	case date, server, host, `cache-control`


case class SigVerificationData(pubKey: PublicKey, sig: Signature) {
	//this is not thread safe!
	def verifySignature(signingStr: String) = (base64SigStr: String) =>
		sig.initVerify(pubKey)
		sig.update(signingStr.getBytes(StandardCharsets.US_ASCII))
		sig.verify(new Base64(base64SigStr).decode())
}

case class SigningData(privateKey: PrivateKey, sig: Signature) {
	//this is not thread safe!
	def sign(bytes: Array[Byte]): Try[Array[Byte]] = Try{
		sig.initSign(privateKey)
		sig.update(bytes)
		sig.sign()
	}

}
