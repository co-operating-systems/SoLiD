package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpEntity, HttpRequest, Uri}
import akka.http.scaladsl.model.headers.{Authorization, GenericHttpCredentials, HttpChallenge, HttpCredentials,
	OAuth2BearerToken, `Content-Type`}
import akka.http.scaladsl.server.AuthenticationFailedRejection.{CredentialsMissing, CredentialsRejected}
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive1, RequestContext}
import akka.http.scaladsl.server.Directives.{AsyncAuthenticator, AuthenticationResult,
	authenticateOrRejectWithChallenge, extract, extractCredentials, extractRequestContext, provide}
import akka.http.scaladsl.server.directives.{AuthenticationDirective, AuthenticationResult, Credentials}
import akka.http.scaladsl.server.directives.BasicDirectives.extractExecutionContext
import akka.http.scaladsl.server.directives.RouteDirectives.reject
import akka.http.scaladsl.util.FastFuture
import com.nimbusds.jose.jwk.JWK
import org.tomitribe.auth.signatures.{Algorithm, Signatures, Signer, SigningAlgorithm, Verifier}
import run.cosy.http.{InvalidCreatedFieldException, InvalidExpiresFieldException}

import java.net.URI
import java.security.{PublicKey, Signature}
import java.time.{Clock, Instant}
import java.util
import java.util.{Locale, Map}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}
import scala.util.matching.Regex

object HttpSig {

	trait Agent
	case class KeyAgent(keyId: Uri) extends Agent
	class Anonymous extends Agent
	object WebServerAgent extends Agent

	val URL: Regex = "<(.*)>".r

	/**
	 *  Implementation of [[https://github.com/solid/authentication-panel/blob/main/proposals/HttpSignature.md Http Signature]] using
	 *  older draft-cavage-12 specification for the moment, as that is what is provided by [[https://github.com/tomitribe/http-signatures-java/issues/51 org.tomitribe's library]].
	 *
	 *  It does not quite work be built this like [[https://doc.akka.io/docs/akka-http/current/routing-dsl/directives/security-directives/authenticateOAuth2Async.html authenticateOAuth2Async]]
	 * which takes an `AsyncAuthenticator` function. The role of the authenticator is to fetch a resource from the Web that will verify
	 * the Credential.  This works for passwords, where there is one source of truth, but not on the web where the Credentials Object
	 * would at least need to contain information about where to fetch the information.
	 *
	 * {{{
	 * type AsyncAuthenticator[T] = Credentials => Future[Option[T]]
	 *
	 * def authenticateOAuth2Async[T](
	 *    realm: String,
	 *    authenticator: AsyncAuthenticator[T]
	 * ): AuthenticationDirective[T]
	 * }}}
	 *
	 * In our case in a way the `Credentials` is the whole HTTP header, even if the `Authorization: Signature ...` header plays
	 * a key role.  What we need is a function to fetch the PublicKeyAlgo from the web (could be local), which we can then use
	 * to verify the signature by checking the HTTP header.
	 *
	 * That would furthermore only give us
	 *
	 *  use with [[akka.http.scaladsl.server.directives.SecurityDirectives.authenticateOrRejectWithChallenge]]
 	 */
	def httpSignature(reqc: RequestContext)(fetch: Uri => Future[SigningData]): AuthenticationDirective[Agent] =
		authenticateOrRejectWithChallenge(httpSigAuthN(reqc.request)(fetch)(using reqc.executionContext))

	def httpSigAuthN(req: HttpRequest)(
		fetch: Uri => Future[SigningData])(
		using ec: ExecutionContext
	): Option[HttpCredentials] => Future[AuthenticationResult[Agent]] =
		case Some(c@GenericHttpCredentials("Signature",_,params)) =>
			//			val sig = Try(Signature.fromString(c.toString()))
			def p(key: String) = params.get(key)
			(p("keyId"),p("algorithm"),p("headers"),p("signature"),p("created"),p("expires")) match
				case (Some(URL(keyId)),Some("hs2019"),Some(headerNames),Some(base64Sig),created,expires) =>
					import scala.jdk.CollectionConverters.SeqHasAsJava , HttpSig.headers
					for {
						sig 			<- HttpSig(headers(headerNames), created, expires)
						signingStr	<- FastFuture(sig.createSigningString(req))
						pka			<- fetch(keyId)
					} yield {
						import AuthenticationResult.{failWithChallenge, success}
						if pka.verifySignature(signingStr)(base64Sig) then
							success(KeyAgent(Uri(keyId)))
						else
							failWithChallenge(HttpChallenge("Signature", None))
					}
				case e => //todo: we need to return some more details on the failure
					FastFuture.successful(AuthenticationResult.failWithChallenge(HttpChallenge("Signature",None)))
		case Some(_) =>
			FastFuture.successful(AuthenticationResult.failWithChallenge(HttpChallenge("Signature",None)))
		case None => //we return an anonymous agent
			FastFuture.successful(AuthenticationResult.success(Anonymous()))
	end httpSigAuthN

	//todo: should be set in preferences
	val maxTimeSkewInMilliseconds = 3 * 60 * 1000L

	/** We create a Signature but verify the time stamp first */
	def apply(headerNames: List[String],
				 creation: Option[String]=None,
				 expiration: Option[String]=None
				): Future[HttpSig] =
		val now = System.currentTimeMillis()
		FastFuture(Try{
			val cTime = creation.map(_.toLong)
			val eTime = expiration.map(_.toLong)
			if cTime.getOrElse(now) > now + maxTimeSkewInMilliseconds then
				throw InvalidCreatedFieldException("Signature is not valid yet")
			else if eTime.getOrElse(now) < now - 10 then
				throw InvalidExpiresFieldException("Signature has expired")
			else new HttpSig(headerNames,cTime,eTime)
		})

	def headers(headerAttribute: String): List[String] =
		headerAttribute.split("\\s+").toList

}

class HttpSig(
	headerNames: List[String],
	signatureCreation: scala.Option[Long],
	signatureExpiration: scala.Option[Long]
) {
	import scala.jdk.CollectionConverters.SeqHasAsJava
	def createSigningString(req: HttpRequest): Try[String] =
		val headersMap = req.headers.foldRight(new java.util.HashMap[String,String]()){
			(h,m) => m.put(h.name(),h.value); m}
		if req.entity != HttpEntity.Empty then
			headersMap.put(`Content-Type`.name,req.entity.contentType.toString())
		Try(Signatures.createSigningString(
			headerNames.asJava, req.method.value, s"<${req.uri.toString()}>",
			headersMap,
			signatureCreation.map(long2Long).orNull,
			signatureExpiration.map(long2Long).orNull
		))
}
