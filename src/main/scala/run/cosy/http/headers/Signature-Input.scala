package run.cosy.http.headers

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.{CustomHeader, RawHeader}
import cats.parse.Parser
import com.nimbusds.jose.util.Base64
import run.cosy.http.headers.Rfc8941
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion, HTTPHeaderParseException}

import java.nio.charset.StandardCharsets
import java.security.{PrivateKey, PublicKey, Signature}
import java.time.Instant
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}



/**
 *  [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-02#section-4.1 4.1 The 'Signature-Input' HTTP header]] defined in "Signing HTTP Messages" HttpBis RFC.
 *  since the only algorithm now is "hs2019" we don't allow anything else to be set
 * @param text
 */
final case class `Signature-Input`(sig: SigHeaders, keyId: String, created: Option[Instant], expires: Option[Instant]) extends BetterCustomHeader[`Signature-Input`]:
	override def renderInRequests = true
	override def renderInResponses = false
	override val companion = `Signature-Input`
	override def value: String = s"""${sig.toString}; keyid="$keyId"; alg="hs2019""""++dateString
	private def dateString: String =
		val cs = created.map(c=> s"; created=${c.toEpochMilli/1000}").getOrElse("")
		val ts = created.map(c=> s"; expires=${c.toEpochMilli/1000}").getOrElse("")
		cs ++ ts


object `Signature-Input` extends BetterCustomHeaderCompanion[`Signature-Input`] :
	override val name = "Signature-Input"
	type SigInput = (SigHeaders, String, Option[Instant], Option[Instant])

	//override try to generalise later
	def parse(value: String): Try[ListMap[Rfc8941.Key,SigInput]] =
		val sig: Try[ListMap[Rfc8941.Key, Rfc8941.Parameterized]] =
			Rfc8941.sfDictionary.parseAll(value) match {
				case Left(e) => Failure(HTTPHeaderParseException(e,value))
				case Right(lm) =>
					val x = lm.map{ case (key,p) =>

						???
					}
					??? //Success(x)
			}
		//		.left.map((e: Parser.Error) => HTTPHeaderParseException(e,value)).toTry
		
		???

	//can an unapply return a Try in scala3
	//override - try to generalise later
	def unapply(h: HttpHeader): Option[(SigHeaders, String, Option[Instant], Option[Instant])] = h match {
		case _: (RawHeader | CustomHeader) =>
			if (h.lowercaseName == lowercaseName) ??? else ??? //parse(h.value.asEncoded).toOption else None
		case _ => None
	}
end `Signature-Input`

case class SigHeaders(name: String, headers: Seq[SigHeaderNames]) {
	override def toString: String = s"""$name=($headersString)"""
	def headersString: String = headers.map(n=>s""""$n"""").mkString(" ")
}

/**
 * Signature Headers supported.
 * Feel free to add new ones by submitting PRs
 **/
enum SigHeaderNames:
	case `@request-target`, `@signature-params`, date, server, `cache-control`

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
