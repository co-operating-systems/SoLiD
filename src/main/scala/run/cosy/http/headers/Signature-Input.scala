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
import run.cosy.http.headers.Rfc8941.{IntStr, PItem, Token}

import scala.collection.immutable


/**
 *  [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-02#section-4.1 4.1 The 'Signature-Input' HTTP header]] defined in "Signing HTTP Messages" HttpBis RFC.
 *  since the only algorithm now is "hs2019" we don't allow anything else to be set
 * @param text
 */
final case class `Signature-Input`(sig: ListMap[Rfc8941.Token,SigInput]) extends BetterCustomHeader[`Signature-Input`]:
	override def renderInRequests = true
	override def renderInResponses = false
	override val companion = `Signature-Input`
	override def value: String = sig.values.mkString(", ")


object `Signature-Input` extends BetterCustomHeaderCompanion[`Signature-Input`]:
	override val name = "Signature-Input"

	//override try to generalise later
	def parse(value: String): Try[ListMap[Rfc8941.Token,SigInput]] =
		val sig: Try[ListMap[Rfc8941.Token, SigInput]] =
			Rfc8941.Parser.sfDictionary.parseAll(value) match {
				case Left(e) => Failure(HTTPHeaderParseException(e,value))
				case Right(lm) =>
					val x = lm.collect{
						case (sigName, _) => 
						???
					}
					??? //Success(x)
			}
		//		.left.map((e: Parser.Error) => HTTPHeaderParseException(e,value)).toTry
		???

	//can an unapply return a Try in scala3
	//override - try to generalise later
	def unapply(h: HttpHeader): Option[SigInput] = h match {
		case _: (RawHeader | CustomHeader) =>
			if (h.lowercaseName == lowercaseName) ??? else ??? //parse(h.value.asEncoded).toOption else None
		case _ => None
	}
end `Signature-Input`

case class SigInput(headers: Seq[HeaderSelector], att: SigAttributes) {

	override def toString: String = s"""($headersString); $att"""
	def headersString: String = headers.map(name=>s""""$name"""").mkString(" ")

	private def dateString: String =
		val cs = att.created.map(c=> s"; created=$c").getOrElse("")
		val ts = att.expires.map(t=> s"; expires=$t").getOrElse("")
		cs ++ ts
}

/**
 * We lump the parameters together as there may be some we can't interpret,
 * but we requure keyId to be present.
 **/
class SigAttributes(keyId: String, params: Rfc8941.Parameters) {
	//for both created and expires we return the time only if the types are correct, otherwise we ignore.
	def created: Option[Long] = params.get(Token("created")).collect{case IntStr(num) => num.toLong}
	def expires: Option[Long] = params.get(Token("expires")).collect{case IntStr(num) => num.toLong}
	override def toString(): String =  ???
}

object SigInput {
	val supported: Map[String, HeaderName] =  HeaderName.values.map(hn => (hn.toString,hn)).toMap
	
	def unapply(iList: Rfc8941.IList): Option[SigInput] = {
		val hdrs: List[HeaderSelector] = iList.items.map{ case PItem(item,params) =>
			item match
			case header: String => supported.get(header).map{ hn =>
				val ip: immutable.Iterable[Selector] = params.map{
					case (Token("key"),Token(k)) => KeySelector(k)
					case (Token("prefix"), IntStr(num)) => PrefixSelector(num.toInt)
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
			case (Token("alg"), "hs2019") => ???
			case (Token("created"), IntStr(t0)) => ???
			case (Token("expires"), IntStr(t1)) => ???
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
	case date, server, `cache-control`

sealed trait Selector
case class KeySelector(key: String) extends Selector
case class PrefixSelector(first: Int) extends Selector
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
