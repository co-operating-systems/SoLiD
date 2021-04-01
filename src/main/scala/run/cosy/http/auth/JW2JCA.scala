package run.cosy.http.auth

import com.nimbusds.jose.JWSAlgorithm
import com.nimbusds.jose.crypto.factories.DefaultJWSSignerFactory
import com.nimbusds.jose.crypto.impl.RSASSA
import com.nimbusds.jose.jwk.{AsymmetricJWK, ECKey, JWK, RSAKey}
import com.nimbusds.jose.util.Base64
import run.cosy.http.CryptoException

import java.nio.charset.{Charset, StandardCharsets}
import java.security.{Provider, PublicKey, Signature}
import scala.util.{Failure, Try}

/**
 * Map JWK Names to Java Cryptography Architecture names
 */
object JW2JCA {
	val signerFactory = new DefaultJWSSignerFactory()

	def jw2rca(jwk: JWK): Try[SigningData] = {
		jwk.getAlgorithm match {
		case jwsAlgo: JWSAlgorithm =>
			Try(RSASSA.getSignerAndVerifier(jwsAlgo,signerFactory.getJCAContext.getProvider)).flatMap{sig=>
				jwk match
				case k: AsymmetricJWK => Try(SigningData(k.toPublicKey, sig))
				case _ => Failure(CryptoException("we only use assymetric keys!"))
			}
		case alg => Failure(CryptoException("We do not support algorithm "+alg))
		}
	}

	/**
	 * Get the java.security.signature for a given JCA Algorithm
	 * todo: build a typesafe library of such algorithms
	 */
	def getSignerAndVerifier(jcaAlg: String, providerOpt: Option[Provider]=None): Try[Signature] =
		Try {
			providerOpt.map(provider => Signature.getInstance(jcaAlg, provider))
				.getOrElse(Signature.getInstance(jcaAlg))
		}


}

case class SigningData(pubKey: PublicKey, sig: Signature) {
	//this is not thread safe!
	def verifySignature(signingStr: String) = (base64SigStr: String) =>
		sig.initVerify(pubKey)
		sig.update(signingStr.getBytes(StandardCharsets.US_ASCII))
		sig.verify(new Base64(base64SigStr).decode())
}

