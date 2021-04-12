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
import run.cosy.http.headers.Rfc8941.{PItem, Key}


/**
 *  [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-02#section-4.1 4.1 The 'Signature-Input' HTTP header]] defined in "Signing HTTP Messages" HttpBis RFC.
 *  since the only algorithm now is "hs2019" we don't allow anything else to be set
 * @param text
 */
final case class `Signature-Input`(sig: ListMap[Rfc8941.Key,SigInput]) extends BetterCustomHeader[`Signature-Input`]:
	override def renderInRequests = true
	override def renderInResponses = false
	override val companion = `Signature-Input`
	override def value: String = sig.values.mkString(", ")


object `Signature-Input` extends BetterCustomHeaderCompanion[`Signature-Input`]:
	override val name = "Signature-Input"

	//override try to generalise later
	def parse(value: String): Try[ListMap[Rfc8941.Key,SigInput]] =
		val sig: Try[ListMap[Rfc8941.Key, SigInput]] =
			Rfc8941.sfDictionary.parseAll(value) match {
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

case class SigInput(headers: Seq[HeaderSelector],
	keyId: String, created: Option[Long], expires: Option[Long]) {

	override def toString: String = s"""($headersString); $attributes"""
	def headersString: String = headers.map(name=>s""""$name"""").mkString(" ")
	def attributes: String = s"""keyid="$keyId"; alg="hs2019""""++dateString

	private def dateString: String =
		val cs = created.map(c=> s"; created=$c").getOrElse("")
		val ts = expires.map(t=> s"; expires=$t").getOrElse("")
		cs ++ ts
}

object SigInput {
	val supported: Map[String, HeaderName] =  HeaderName.values.map(hn => (hn.toString,hn)).toMap
	
	def unapply(iList: Rfc8941.IList): Option[SigInput] = {
		val hdrs = iList.items.map{ case PItem(item,params) =>
			item match
			case header: String => supported.get(header).map{ hn =>
//					params.map match {
//						case (Key("key"),_) => ???
//					}
				}.getOrElse(unsupported)
			case _ => unsupported
		}
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
 */
case class HeaderSelector(header: HeaderName, selectors: List[Selector] = List())

/**
 * Signature Headers supported.
 * Feel free to add new ones by submitting PRs
 *
 * todo: is this pushing typesafety too far?
 **/
enum HeaderName(val special: Boolean = false):
	case `@request-target` extends HeaderName(true)
	case `@signature-params` extends HeaderName(true)
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
