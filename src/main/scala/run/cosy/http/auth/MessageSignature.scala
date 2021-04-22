package run.cosy.http.auth

import akka.http.javadsl.model.headers.Host
import akka.http.scaladsl.model.headers.{Authorization, CustomHeader, Date, ETag, GenericHttpCredentials, HttpChallenge, HttpCredentials, RawHeader, `Cache-Control`, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpHeader, HttpMessage, HttpRequest, HttpResponse, Uri}
import com.nimbusds.jose.util.Base64
import run.cosy.http.headers.{HeaderName, Rfc8941, SigInput, Signature, Signatures, `Signature-Input`}
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion, InvalidSigException, UnableToCreateSigHeaderException}
import run.cosy.http.headers.Rfc8941._
import akka.http.scaladsl.server.Directives.AuthenticationResult
import akka.http.scaladsl.server.directives.AuthenticationResult
import akka.http.scaladsl.server.directives.AuthenticationResult.{failWithChallenge, success}
import akka.http.scaladsl.util.FastFuture
import run.cosy.http.auth.HttpSig.{Agent, KeyAgent}

import java.nio.charset.{Charset, StandardCharsets}
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
	/**
	 * [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-4.1 Message Signatures]]
	 */
	extension[T <: HttpMessage](msg: T) {

		/**
		 * Generate a proces to create a new HttpRequest with the given Signature-Input header.
		 * Called by the client, building the request.
		 *
		 * @param sigInput header describing the headers to sign as per "Signing Http Messages" RFC
		 * @return a function to create a new HttpRequest when given signing data wrapped in a Try
		 *         the Try can capture an IllegalArgumentException if the required headers are not present
		 *         in the request
		 */
		def withSigInput(name: Rfc8941.Token, sigInput: SigInput): Try[SigningData => Try[msg.Self]] =
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
		def signingString(sigInput: SigInput): Try[String] =
			import Rfc8941.Serialise.{given, _}
			import run.cosy.http.headers.{HeaderName => HN}

			def mkHdr(name: PItem[SfString], values: Seq[HttpHeader]): Try[String] = {
				//todo: other reasons to return None are that attributes don't select into header correctly
				if values.isEmpty then Failure(new UnableToCreateSigHeaderException("no headers for " + name))
				else Success(name.canon + ": " + values.map(_.value).mkString(", "))
			}

			@tailrec
			def buildSigString(todo: Seq[Rfc8941.PItem[SfString]], onto: String): Try[String] =
				if todo.isEmpty then Success(onto)
				else
					val pih = todo.head
					val tt = HeaderName.valueOf(pih.item.asciiStr) match
						case HN.date => mkHdr(pih, msg.headers[Date])
						case HN.`cache-control` => mkHdr(pih, msg.headers[`Cache-Control`])
						case HN.etag => mkHdr(pih, msg.header[ETag].toSeq)
						case HN.host => mkHdr(pih, msg.header[Host].toSeq)
						case HN.`@signature-params` => Success(pih.canon + ": " + sigInput.il.canon)
						case HN.`@request-target` =>
							msg match
								case req: HttpRequest => Success(pih.canon + ": " +
									req.method.value.toLowerCase(Locale.ROOT) + " " + req.uri.path + {
									req.uri.rawQueryString.map("?" + _).getOrElse("")
								})
								case _: HttpResponse => Failure(
									new UnableToCreateSigHeaderException("cannot build @request-target for response message"))
					tt match
						case Success(hdr) => buildSigString(todo.tail, if onto == "" then hdr else onto + "\n" + hdr)
						case f => f
				end if
			end buildSigString

			buildSigString(sigInput.headerItems.appended(PItem(SfString(HN.`@signature-params`.toString))), "")
		end signingString

		/** get the signature data for a given signature name
		 *
		 * @return a pair of SigInput Data, and the signature bytes
		 *         The SigInput Data tells us what the signature bytes are a signature of
		 *         and how to interpret them, i.e. what the headers are that were signed, where
		 *         the key is and what the signing algorithm used was
		 * */
		def getSignature(name: Rfc8941.Token): Option[(SigInput, Bytes)] =
			import msg.headers
			headers.collectFirst {
				case `Signature-Input`(inputs) if inputs.get(name).isDefined =>
					inputs.get(name).get
			}.flatMap { siginput =>
				headers.collectFirst {
					case Signature(sigs) if sigs.get(name).isDefined => (siginput, sigs.get(name).get)
				}
			}
	}

	extension (req: HttpRequest) {

		/**
		 * lift a function to fetch the keyId, into a partial function which given an HttpCredential
		 * will return an Authentication result.
		 * The HttpCredential is in the request, but this is useful for interacting with Akka's directives
		 * todo: see if one can narrow the domain GenericHttpCrendtials to exactly HttpSig credentials
		 *    so that we have a total function again
		 **/
		def signatureAuthN(
			fetchKeyId: Uri => Future[SigVerificationData]
		)(using
			ec: ExecutionContext, clock: Clock
		): PartialFunction[GenericHttpCredentials,Future[Agent]] =
			case GenericHttpCredentials("HttpSig", _, params) =>
				val tr = for {
					name <- params.get("name")
							.toRight(InvalidSigException("HttpSig auth needs name parameter")).toTry
					(si: SigInput, sig: Bytes) <- req.getSignature(Rfc8941.Token(name))
							.toRight(InvalidSigException(
								s"could not find Signature-Input and Signature for Sig name '$name' ")
							).toTry
					if si.isValidAt(clock.instant)
					sigStr <- req.signingString(si) //we should keep all as Try and add error msgs
				} yield (si, sigStr, sig)
				// now we have all the data
				for {
					(si: SigInput, sigStr: String, sig: Bytes) <- FastFuture(tr)
					sigVer <- fetchKeyId(si.keyid)
					if sigVer.verifySignature(sigStr)(sig)
				} yield KeyAgent(si.keyid)
	}

}