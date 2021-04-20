package run.cosy.http.auth

import akka.http.javadsl.model.headers.Host
import akka.http.scaladsl.model.headers.{CustomHeader, Date, ETag, RawHeader, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpMessage, HttpRequest, HttpResponse}
import com.nimbusds.jose.util.Base64
import run.cosy.http.headers.{HeaderName, Rfc8941, SigInput, Signature, Signatures, `Signature-Input`}
import run.cosy.http.UnableToCreateSigHeaderException
import Rfc8941._
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion}

import java.nio.charset.{Charset, StandardCharsets}
import java.security.{PrivateKey, PublicKey}
import java.time.Instant
import java.util.Locale
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object MessageSignature {
	/**
	 * [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-4.1 Message Signatures]]
	 */
	extension (msg: HttpMessage)

	/**
	 * generate a proces to create a new HttpRequest with the given Signature-Input header.
	 * Called by the client, building the request.
	 * (Q: should this be on the RequestBuilder also or instead?)
	 *
	 * @param sigInput header describing the headers to sign as per rfc
	 * @return a function to create a new HttpRequest when given signing data wrapped in a Try
    *   the Try can capture an IllegalArgumentException if the required headers are not present
	 *   in the request
	 */
		def withSigInput(name: Rfc8941.Token, sigInput: SigInput): Try[SigningData => Try[HttpMessage]] =
			signingString(sigInput).map { sigString =>
				(sigData: SigningData) =>
					sigData.sign(sigString.getBytes(StandardCharsets.US_ASCII)).map { sigbytes =>
						import akka.http.scaladsl.model.{Uri, UriRendering}
						import UriRendering.given
						msg.withHeaders(msg.headers.appendedAll(Seq(
							`Signature-Input`(name, sigInput),
							Signature(Signatures(name, collection.immutable.ArraySeq.unsafeWrapArray(sigbytes)))))
							)
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
			import run.cosy.http.headers.{HeaderName => hn}
			import Rfc8941.Serialise.{given, _}

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
					val tt = hn.valueOf(pih.item.asciiStr) match
						case hn.date => mkHdr(pih, msg.headers[Date])
						case hn.`cache-control` => mkHdr(pih, msg.headers[`Cache-Control`])
						case hn.etag => mkHdr(pih, msg.header[ETag].toSeq)
						case hn.host => mkHdr(pih, msg.header[Host].toSeq)
						case hn.`@signature-params` => Success(pih.canon + ": " + sigInput.il.canon)
						case hn.`@request-target` =>
							msg match
								case req: HttpRequest => Success(pih.canon + ": " +
									req.method.value.toLowerCase(Locale.ROOT) + " " + req.uri.path + {
									req.uri.rawQueryString.map("?" + _).getOrElse("")
								})
								case _: HttpResponse => Failure(
									new UnableToCreateSigHeaderException("cannot build @request-target for response message"))
					tt match
						case Success(hdr) => buildSigString(todo.tail, if onto=="" then hdr else onto + "\n" + hdr)
						case f => f
				end if
			end buildSigString

			buildSigString(sigInput.headerItems.appended(PItem(SfString(hn.`@signature-params`.toString))), "")
		end signingString

}