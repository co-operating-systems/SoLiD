package run.cosy.http.auth

import akka.http.scaladsl.model.Uri
import run.cosy.http.InvalidSigException
import run.cosy.http.auth.{KeyidAgent, KeyIdAgent}
import run.cosy.http.headers.Rfc8941
import run.cosy.http.headers.Rfc8941.Bytes

import java.nio.charset.StandardCharsets
import java.security.{PrivateKey, PublicKey, SignatureException, Signature => JSignature}
import scala.util.{Failure, Success, Try}
import scala.util

/**
 * SignatureVerifier is essentially a function that verifies that a signing string is signed
 * by the signature in bytes.
 * (There are many algorithms to verify a signature but this trait abstracts them all.)
 * This goes beyond a simple function though: it is Object Oriented in the CoAlgebraic sense,
 * in that it gives back the observation of the identity of the Agent as a String identifier.
 * This is only secure if the KeyId is tied to the signature proof.
 *
 * @returns a verified keyid, if the signature is good, or a failure explaining the problem.
 *   the implementation of Keyid can contain more informaiton than the keyid but must contain
 *   at least that.
 **/
trait SignatureVerifier[T<:Keyid]:
	def verifySignature(signingStr: String, signature: Rfc8941.Bytes): Try[T]

object SignatureVerifier {
	/**
	 * this just returns a verification function on signatures and signing strings.
	 * Note that JSignature objects are not thread safe: so this should be wrapped in some IO Monad,
	 * if one were strict.
	 **/
	def jsigVerifier(pubKey: PublicKey, sig: JSignature): (String, Rfc8941.Bytes) => Try[Unit] =
		(signingStr, signature) =>
			try {
				sig.initVerify(pubKey)
				sig.update(signingStr.getBytes(StandardCharsets.US_ASCII))
				if sig.verify(signature.toArray) then Success(())
				else Failure(InvalidSigException(s"cannot verify signature"))
			} catch {
				case e: SignatureException => Failure(e)
			}

	/**
	 * given a keyId Uri, a public key and a signature algorithm return a
	 * verifier that will return an WebKeyIdAgent for verified signatures.
	 * The pubkey and algorithm must come from a request to the keyId.
	 * So arguably those three arguments should be wrapped in one object constituting
	 * a proof of them being fetched from that URI.
	 * Either that or the function should be useable only from those contexts.
	 * One may want to model this even more strictly by having the returned agent keep the full proof of the algorith
	 * (at least the name), as say different strenght of hashes will give different levels of confidence in the idenity.
	 *
	 * We call this apply, because for the HttpSig framework all the time
	 **/
	def apply(keydId: Uri, pubKey: PublicKey, sigAlgorithm: JSignature): SignatureVerifier[KeyIdAgent] =
		new SignatureVerifier[KeyIdAgent] {
			override def verifySignature(signingStr: String, signature: Rfc8941.Bytes): Try[KeyIdAgent] =
				jsigVerifier(pubKey,sigAlgorithm)(signingStr,signature)
					.map(_ => KeyIdAgent(keydId, pubKey))
		}

	/** the same as pubKeyIdUriVerifier, but for protocols that don't consider the keyID to be a Uri */
	def keyidVerifier(keydId: Rfc8941.SfString, pubKey: PublicKey, sigAlg: JSignature): SignatureVerifier[KeyidAgent] =
		new SignatureVerifier[KeyidAgent] {
			override def verifySignature(signingStr: String, signature: Rfc8941.Bytes): Try[KeyidAgent] =
				jsigVerifier(pubKey,sigAlg)(signingStr,signature)
					.map(_ => KeyidAgent(keydId.asciiStr, pubKey))
		}

}

/**
 * Used for Signing bytes.
 **/
case class SigningData(privateKey: PrivateKey, sig: JSignature):
	//this is not thread safe!
	def sign(bytes: Array[Byte]): Try[Array[Byte]] = Try {
		sig.initSign(privateKey)
		sig.update(bytes)
		sig.sign()
	}
