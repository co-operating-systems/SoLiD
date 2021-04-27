package run.cosy.http.auth

import akka.http.javadsl.model.headers.Host
import akka.http.scaladsl.model.headers.{Authorization, CustomHeader, Date, ETag, GenericHttpCredentials, HttpChallenge, HttpCredentials, RawHeader, `Cache-Control`, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpHeader, HttpMessage, HttpRequest, HttpResponse, Uri}
import com.nimbusds.jose.util.Base64
import run.cosy.http.headers.{Rfc8941, SigInput, Signature, Signatures, `Signature-Input`,SelectorOps,`@signature-params`}
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion, InvalidSigException, UnableToCreateSigHeaderException}
import run.cosy.http.headers.Rfc8941._
import akka.http.scaladsl.server.Directives.AuthenticationResult
import akka.http.scaladsl.server.directives.AuthenticationResult
import akka.http.scaladsl.server.directives.AuthenticationResult.{failWithChallenge, success}
import akka.http.scaladsl.util.FastFuture
import run.cosy.http.auth.{Agent, KeyidAgent}

import java.nio.charset.StandardCharsets
import java.security.{PrivateKey, PublicKey}
import java.time.{Clock, Instant}
import java.util.Locale
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Adds extensions methods to sign HttpMessage-s - be they requests or responses.
 **/
object MessageSignature {
	val urlStrRegex = "<(.*)>".r
	
	/**
	 * [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-4.1 Message Signatures]]
	 */
	extension[T <: HttpMessage](msg: T) {

		/**
		 * Generate a proces to create a new HttpRequest with the given Signature-Input header.
		 * Called by the client, building the message.
		 *
		 * @param sigInput header describing the headers to sign as per "Signing Http Messages" RFC
		 * @return a function to create a new HttpRequest when given signing data wrapped in a Try
		 *         the Try can capture an IllegalArgumentException if the required headers are not present
		 *         in the request
		 */
		def withSigInput(
			name: Rfc8941.Token, sigInput: SigInput
		)(using selector: SelectorOps[HttpMessage]): Try[SigningData => Try[msg.Self]] =
			signingString(sigInput).map { sigString =>
				(sigData: SigningData) =>
					sigData.sign(sigString.getBytes(StandardCharsets.US_ASCII)).map { sigbytes =>
						import akka.http.scaladsl.model.{Uri, UriRendering}
						import UriRendering.given
						import scala.jdk.CollectionConverters.given
						msg.addHeaders(Seq(
							`Signature-Input`(name, sigInput),
							Signature(Signatures(name, collection.immutable.ArraySeq.unsafeWrapArray(sigbytes)))
						).asJava)
					}
			}
		end withSigInput

		/**
		 * Generate the signature string, given the `signature-input` header.
		 * This is to be called the server to verify a signature,
		 * but also by `withSigInput` to generate the signature.
		 * Note, that the headers to be signed, always contains the `signature-input` header itself.
		 *
		 * @param sigInput the sigInput header specifying the
		 * @return signing String for given Signature Input on this http requets.
		 *         This string will either need to be verified with a public key against the
		 *         given one, or will need to be signed to be added to the Request.
		 *         In the latter case use the withSigInput method.
		 *         todo: it may be more correct if the result is a byte array, rather than a Unicode String.
		 */
		def signingString(sigInput: SigInput)(using selector: SelectorOps[HttpMessage]): Try[String] =
			import Rfc8941.Serialise.{given, _}

			@tailrec
			def buildSigString(todo: Seq[Rfc8941.PItem[SfString]], onto: String): Try[String] =
				if todo.isEmpty then Success(onto)
				else
					val pih = todo.head
					if (pih == `@signature-params`.pitem) then
						val sigp  =`@signature-params`.signingString(sigInput)
						Success(if onto == "" then sigp else onto + "\n" + sigp)
					else selector.select(msg,pih) match
						case Success(hdr) => buildSigString(todo.tail, if onto == "" then hdr else onto + "\n" + hdr)
						case f => f
				end if
			end buildSigString

			buildSigString(sigInput.headerItems.appended(`@signature-params`.pitem), "")
		end signingString

		/** get the signature data for a given signature name
		 *
		 * @return a pair of SigInput Data, and the signature bytes
		 *         The SigInput Data tells us what the signature bytes are a signature of
		 *         and how to interpret them, i.e. what the headers are that were signed, where
		 *         the key is and what the signing algorithm used was
		 * */
		def getSignature(name: Rfc8941.Token)(using selector: SelectorOps[HttpMessage]): Option[(SigInput, Bytes)] =
			import msg.headers
			headers.collectFirst {
				case `Signature-Input`(inputs) if inputs.get(name).isDefined =>
					inputs.get(name).get
			}.flatMap { siginput =>
				headers.collectFirst {
					case Signature(sigs) if sigs.get(name).isDefined => (siginput, sigs.get(name).get)
				}
			}

		/**
		 * lift a function to fetch the keyId, into a partial function which given an HttpCredential
		 * will return an Authentication result.
		 * The HttpCredential is in the request, but this is useful for interacting with Akka's directives
		 * todo: see if one can narrow the domain GenericHttpCrendtials to exactly HttpSig credentials
		 *    so that we have a total function again
		 **/
		def signatureAuthN[Kid <: Keyid](
			fetchKeyId: Rfc8941.SfString => Future[SigVerification[Kid]]
		)(using
			ec: ExecutionContext, clock: Clock, so : SelectorOps[HttpMessage]
		): PartialFunction[GenericHttpCredentials,Future[Kid]] =
			case GenericHttpCredentials("HttpSig", _, params) =>
				val tr = for {
					name <- params.get("proof")
							.toRight(InvalidSigException("HttpSig auth needs 'proof' parameter with value the sig name")).toTry
					tkName <- Try(Rfc8941.Token(name)) //todo: tighter input function should move this test out of here
					(si: SigInput, sig: Bytes) <- msg.getSignature(tkName)
							.toRight(InvalidSigException(
								s"could not find Signature-Input and Signature for Sig name '$name' ")
							).toTry
					if si.isValidAt(clock.instant)
					sigStr <- msg.signingString(si)
				} yield (si, sigStr, sig)
				// now we have all the data
				for {
					(si: SigInput, sigStr: String, sig: Bytes) <- FastFuture(tr)
					sigVer <- fetchKeyId(si.keyid)
					agent <- FastFuture(sigVer.verifySignature(sigStr)(sig))
				} yield agent
	}

}