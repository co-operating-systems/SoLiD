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

import scala.collection.immutable


/**
 * [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-4.1 4.1 The 'Signature-Input' HTTP header]]
 * defined in "Signing HTTP Messages" HttpBis RFC.
 * Since version 03 signature algorithms have been re-introduced, but we only implement "hs2019" for simplicity.
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

	def apply(name: Rfc8941.Token, sigInput: SigInput): `Signature-Input` = `Signature-Input`(SigInputs(name,sigInput))

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
	def append(more: SigInputs): SigInputs = new SigInputs(si ++ more.si)
	def append(key: Rfc8941.Token, sigInput: SigInput): SigInputs = new SigInputs(si + (key -> sigInput))
}

object SigInputs:
	/* create a SigInput with a single element */
	def apply(name: Rfc8941.Token,siginput: SigInput) = new SigInputs(ListMap(name->siginput))

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
	import Rfc8941.Serialise.given
	import SigInput.{createdTk,expiresTk,keyidTk,algTk, nonceTk}
	def headers: Seq[HeaderName] = il.items.map{ case PItem(SfString(str),_) => HeaderName.valueOf(str)}
	def headerItems: Seq[PItem[SfString]] = il.items.map(_.asInstanceOf[PItem[SfString]])

	def keyid: Uri =
		import SigInput.urlStrRegex
		val SfString(urlStrRegex(key))  = il.params(keyidTk)
		Uri(key)

	def alg: Option[String] = il.params.get(algTk).collect{case SfString(str) => str}
	def created: Option[Long] = il.params.get(createdTk).collect{case SfInt(time) => time}
	def expires: Option[Long] = il.params.get(expiresTk).collect{case SfInt(time) => time}
	def nonce: Option[String] = il.params.get(nonceTk).collect{case SfString(str) => str}

	def isValidAt(i: Instant, shift: Long=0): Boolean =
		created.map(_ - shift <= i.getEpochSecond).getOrElse(true) &&
			expires.map(_ +shift >= i.getEpochSecond).getOrElse(true)

	def canon: String = il.canon

}

object SigInput {
	val urlStrRegex = "<(.*)>".r

	/** registered metadata parameters as per [[https://www.ietf.org/archive/id/draft-ietf-httpbis-message-signatures-04.html#section-5.2.2 §5.2.2]].
	 * To avoid them being confused with pattern matches variables enclose in ` `.*/
	val algTk = Token("alg")
	val createdTk = Token("created")
	val expiresTk = Token("expires")
	val keyidTk = Token("keyid")
	val nonceTk = Token("nonce")

	val registeredParams = Seq(algTk,createdTk,expiresTk,keyidTk,nonceTk)

	/** as per [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-2.4.2 §2.4.2]] the
	   list of headers MUST NOT include the @signature-params speciality content identifier */
	val acceptedHeadersMap: Map[String, HeaderName] = {
		HeaderName.values.toSeq.filter(_ != HeaderName.`@signature-params`)
			.map(hn => (hn.toString, hn)).toMap
	}

	val Empty = ListMap.empty[Token, Item]

	def apply(il: IList): Option[SigInput] = if valid(il) then Some(new SigInput(il)) else None

	//this is really functioning as a constructor in pattern matching contexts
	def unapply(pzd: Parameterized): Option[SigInput] =
		pzd match
			case il: IList if valid(il) => Some(new SigInput(il))
			case _                      => None


	/**
	 * A Valid SigInput IList has a Interal list of parameterless Strings (We don't
	 * support parameters just yet) taken from the enum of supported headers,
	 * and the list must have a keyid attribute.
	 * These requirements are for [[https://github.com/solid/authentication-panel/blob/main/proposals/HttpSignature.md HTTPSig]]. The Signing Http Messages spec is more lenient and also more general.
	 * */
	def valid(il: IList): Boolean =
		val headersOk = il.items.forall { itm =>
			itm match
				//we don't support attribute selectors on headers yet
				case PItem(SfString(hdr), Empty) if acceptedHeadersMap.contains(hdr) => true
				case _ => false
		}
		//we need "keyid".
		//todo: The restriction on "<" and ">" should really be left to calling code for a more general library
		val paramsOk = il.params.forall {
			case (`keyidTk`, item: SfString) =>
				val cl = item.asciiStr.trim()
				cl.startsWith("<") && cl.endsWith(">")
			case (`createdTk`, _: SfInt) | (`expiresTk`, _: SfInt)   => true
			case (`algTk`, _: SfString)  | (`nonceTk`, _: SfString)  => true
			// we are lenient on non-registered params
			case (attr, _) => !registeredParams.contains(attr)
		}
		headersOk && paramsOk
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
	case date, host, etag, `cache-control`

