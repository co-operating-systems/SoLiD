package run.cosy.http.auth

import akka.http.scaladsl.model.Uri
import run.cosy.http.InvalidSigException
import run.cosy.http.auth.{KeyidAgent, WebKeyidAgent}
import run.cosy.http.headers.Rfc8941

import java.nio.charset.StandardCharsets
import java.security.{PrivateKey, PublicKey, Signature=>JSignature}
import scala.util.{Failure, Success, Try}

trait SigVerification[T<:Keyid]:
	def verifySignature(signingStr: String): (sigBytes: Rfc8941.Bytes) => Try[T]

case class RFCSigVerificationData(pubKey: PublicKey, sig: JSignature)(keyid: Rfc8941.SfString)
	extends SigVerification[Keyid]:
	override def verifySignature(signingStr: String) = (sigBytes: Rfc8941.Bytes) => Try {
		sig.initVerify(pubKey)
		sig.update(signingStr.getBytes(StandardCharsets.US_ASCII))
		sig.verify(sigBytes.toArray)
	}.flatMap{b =>
		if b then Success(KeyidAgent(keyid.asciiStr))
		else Failure(InvalidSigException(s"could not verify key signature for $keyid"))
	}

case class SigVerificationData(pubKey: PublicKey, sig: JSignature)(keyId: Uri)
	extends SigVerification[WebKeyidAgent]:
	//this is not thread safe!
	override def verifySignature(signingStr: String) = (sigBytes: Rfc8941.Bytes) => Try {
		sig.initVerify(pubKey)
		sig.update(signingStr.getBytes(StandardCharsets.US_ASCII))
		sig.verify(sigBytes.toArray)
	}.flatMap{b =>
		if b then Success(WebKeyidAgent(keyId))
		else Failure(InvalidSigException(s"could not verify key signature for $keyId"))
	}


case class SigningData(privateKey: PrivateKey, sig: JSignature):
	//this is not thread safe!
	def sign(bytes: Array[Byte]): Try[Array[Byte]] = Try {
		sig.initSign(privateKey)
		sig.update(bytes)
		sig.sign()
	}
