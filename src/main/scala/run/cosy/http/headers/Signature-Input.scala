package run.cosy.http.headers

import akka.http.scaladsl.model.HttpHeader
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
import Rfc8941.{IList, Item, PItem, SfInt, SfList, SfString, Token, SfDict}

import scala.collection.immutable


/**
 *  [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-02#section-4.1 4.1 The 'Signature-Input' HTTP header]] defined in "Signing HTTP Messages" HttpBis RFC.
 *  since the only algorithm now is "hs2019" we don't allow anything else to be set
 * @param text
 */
final case class `Signature-Input`(sig: SigInputs) extends BetterCustomHeader[`Signature-Input`]:
	override def renderInRequests = true
	override def renderInResponses = false
	override val companion = `Signature-Input`
	override def value: String = sig.canonical


object `Signature-Input` extends BetterCustomHeaderCompanion[`Signature-Input`]:
	override val name = "Signature-Input"

	def parse(value: String): Try[SigInputs] =
		Rfc8941.Parser.sfDictionary.parseAll(value) match
			case Left(e) => Failure(HTTPHeaderParseException(e,value))
			case Right(lm) => Success(SigInputs.filterValid(lm))

	//can an unapply return a Try in scala3
	//override - try to generalise later
	def unapply(h: HttpHeader): Option[SigInputs] = h match {
		case _: (RawHeader | CustomHeader) =>
			if (h.lowercaseName == lowercaseName) ??? else ??? //parse(h.value.asEncoded).toOption else None
		case _ => None
	}
end `Signature-Input`

/**
 * A Signature-Input is an SfDictionary whose values are all Internal Lists
 * We don't here verify the structure of this Ilist at this point leaving it
 * to the application layer
 *
 * @param value
 * @return
 */
case class SigInputs(si: ListMap[Rfc8941.Token,SigInput]) {
	import Rfc8941.Serialise._

	def canonical: String = ??? //(si.asInstanceOf[SfDict]).canon

}

object SigInputs {
	import Rfc8941.SfString
	/**
	 * Filter out the inputs that this framework does not accept.
	 * Since this may change with implementations, this should really
	 * be a function provided in a trait provided by the library doing the
	 * verification, so that it can evolve independently of the Http Header
	 * framework.
	 * Todo: make this independent as described above! (when it functions)
	 **/
	def filterValid(lm: SfDict): SigInputs = SigInputs(lm.collect{
		case (sigName, il: IList) if valid(il) => (sigName,SigInput(il))
	})
	/**
	 * A Valid SigInpu IList has a Interal list of parameterless Strings (We don't support those just yet)
	 * and the list has attributes including a keyid, and a protocol. Can have more.
	 **/
	def valid(il: IList): Boolean =
		val headersOk = il.items.forall{ itm =>
			val Empty = ListMap.empty[Token,Item]
			itm match
				case PItem(SfString(hdr),Empty) if acceptedHeadersMap.contains(hdr) => true
				case _ => false
		}
		val (keyIdExists, algoExists) = il.params.foldRight((false,false)){ case (param,(keyIdE, algoE)) =>
			param match
				case (Token("keyid"),SfString(id)) if id.startsWith("<") && id.endsWith(">") => (true,algoE)
				case (Token("alg"),SfString("hs2019")) => (keyIdE,true)
				case _ => (keyIdE,algoE)
		}
		headersOk && keyIdExists && algoExists

	val acceptedHeadersMap = HeaderName.values.toSeq.map(hn=> (hn.toString,hn)).toMap
}

case class SigInput(il: IList) {
	def headers = ??? //il.items.map(_.item.)
}

object SigInput {
	val supported: Map[String, HeaderName] =  HeaderName.values.map(hn => (hn.toString,hn)).toMap
	val hs2019 = SfString("hs2019")

	def unapply(iList: Rfc8941.IList): Option[SigInput] = {
		val hdrs: List[HeaderSelector] = iList.items.map{ case PItem(item,params) =>
			item match
			case header: String => supported.get(header).map{ hn =>
				val ip: immutable.Iterable[Selector] = params.map{
					case (Token("key"),Token(k)) => KeySelector(k)
					case (Token("prefix"), num: SfInt) => PrefixSelector(num)
					case _ => unsupported
				}
				if ip.exists(_ == unsupported) then UnimplementedSelector
				else HeaderSelector(hn,ip.toList)
			}.getOrElse(UnimplementedSelector)
			case _ => UnimplementedSelector
		}
		if hdrs.contains(UnimplementedSelector) then None
		else iList.params.map{
			case (Token("keyid"),id: String) => ???
			case (Token("alg"), hs2019 ) => ???
			case (Token("created"), t0: SfInt) => ???
			case (Token("expires"), t1: SfInt) => ???
			case _ =>  ???
		}
		//this seems too strict. What if there are more attributes in the message received on the signature?
		//If new attributes, don't break the algorithm, but refine it, then we may still be able to verify
		//the siganture. If we drop attributes then we won't be able to do that.
		???
	}
	//		flatMap{
//			case PItem(item: String, attributes) if HeaderSelector.values.contains(item) => List(item)
//			case _ => List()
//		}
}


/**
 * We lump the parameters together as there may be some we can't interpret,
 * but we requure keyId to be present.
 **/
class SigAttributes(keyId: String, params: Rfc8941.Params) {
	import Rfc8941.given
	//for both created and expires we return the time only if the types are correct, otherwise we ignore.
	def created: Option[Long] = params.get(Token("created")).collect{case num: SfInt => num.long}
	def expires: Option[Long] = params.get(Token("expires")).collect{case num: SfInt => num.long}
	override def toString(): String =  ???
}


/**
 * Headers can have attributes used to determine
 *  + [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-2.2 Disctionary Structured Field Members]]
 *  + [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-2.3 List Prefixes]]
 * @param header to select
 * @param selectors ordered list of attributes to select from that header. Order is very important here.
 * todo: it may be better to have HeaderSlectors that each work with a HttpHeader                  
 */
case class HeaderSelector(header: HeaderName, selectors: List[Selector] = List())
object UnimplementedSelector extends HeaderSelector(HeaderName.`@unimplemented`,List())
/**
 * Signature Headers supported.
 * Feel free to add new ones by submitting PRs
 *
 * todo: is this pushing typesafety too far?
 **/
enum HeaderName(val special: Boolean = false):
	case `@request-target` extends HeaderName(true)
	case `@signature-params` extends HeaderName(true)
	case `@unimplemented` extends HeaderName(true)
	case date, server, host, `cache-control`

sealed trait Selector
case class KeySelector(key: String) extends Selector
case class PrefixSelector(first: SfInt) extends Selector
object unsupported extends Selector

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
