package run.cosy.http.auth

import akka.http.scaladsl.model.headers.{CustomHeader, RawHeader}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest}
import com.nimbusds.jose.util.Base64
import run.cosy.http.headers.{SigningData, `Signature-Input`}
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion}

import java.nio.charset.StandardCharsets
import java.security.{PrivateKey, PublicKey, Signature}
import java.time.Instant
import java.util.Locale
import scala.util.Try

/**
 * Sketch of  how to implement [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-02#section-4.1 Message Signatures]] as an extension method
 */
extension (req: HttpRequest)
	/**
	 * generate a proces to create a new HttpRequest with the given Signature-Input header.
	 * Called by the client, building the request.
	 * (Q: should this be on the RequestBuilder also or instead?)
 	 * @param sigInput header describing the headers to sign as per rfc
	 * @return a function to create a new HttpRequest when given signing data.
	 * @throws an IllegalArgumentException if the required headers are not present in the request
	 *            (wrap with a Try in cases where requests are dynamically generated)
	 */
	@throws[java.lang.IllegalArgumentException]
	def withSigInput(sigInput: `Signature-Input`):  SigningData => Try[HttpRequest] = ???

	/**
	 * Generate the signature string, given the `signature-input` header.
	 * This is to be called the server to verify a signature,
	 * but also by `withSigInput` to generate the signature.
	 * Note, that the headers to be signed, always contains the `signature-input` header itself.
	 * @param sigInput the sigInput header specifying the
	 * @return signing String for given Signature Input on this http requets.
	 *         This string will either need to be verified with a public key against the
	 *         given one, or will need to be signed to be added to the Request.
	 *         In the latter case use the withSigInput method.
	 * todo: it may be more correct if the result is a byte array, rather than a Unicode String.
	 */
	def signingString(sigInput: `Signature-Input`): Try[String] = ???


