package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpEntity, HttpHeader, HttpMessage, HttpRequest, Uri}
import akka.http.scaladsl.model.headers.{`Content-Type`, Authorization, GenericHttpCredentials, HttpChallenge, HttpCredentials, OAuth2BearerToken}
import akka.http.scaladsl.server.AuthenticationFailedRejection.{CredentialsMissing, CredentialsRejected}
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Directive1, RequestContext}
import akka.http.scaladsl.server.Directives.{authenticateOrRejectWithChallenge, extract, extractCredentials, extractRequestContext, provide, AsyncAuthenticator, AuthenticationResult}
import akka.http.scaladsl.server.directives.{AuthenticationDirective, AuthenticationResult, Credentials}
import akka.http.scaladsl.server.directives.BasicDirectives.extractExecutionContext
import akka.http.scaladsl.server.directives.RouteDirectives.reject
import akka.http.scaladsl.util.FastFuture
import com.nimbusds.jose.jwk.JWK
import com.nimbusds.jose.util.Base64
import run.cosy.http.auth.{KeyidSubj, KeyIdAgent}
import run.cosy.http.headers.{HSCredentials, HttpSig, Rfc8941}
import run.cosy.http.{InvalidCreatedFieldException, InvalidExpiresFieldException, InvalidSigException}

import java.net.URI
import java.security.{PrivateKey, PublicKey, Signature => JSignature}
import java.time.{Clock, Instant}
import java.util
import java.util.Locale
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex



object HttpSigDirective {
	import run.cosy.http.headers.given
	import run.cosy.http.auth.MessageSignature._
	import run.cosy.http.headers.SelectorOps

	/**
	 * lifts a request context into an authentication directive.
	 * @param reqc
	 * @param fetch
	 * @param _
	 * @return
	 */
	def httpSignature(reqc: RequestContext)(
		fetch: Uri => Future[SignatureVerifier[KeyIdAgent]]
	)(using SelectorOps[HttpMessage]): AuthenticationDirective[KeyIdAgent] = {
		given ec: ExecutionContext = reqc.executionContext
		given cl: Clock = java.time.Clock.systemUTC()
		authenticateOrRejectWithChallenge { 
			case Some(HSCredentials(httpsig)) =>
				reqc.request.signatureAuthN[KeyIdAgent](keyIdtoUri.andThen(_.flatMap(fetch)))(httpsig)
					.map(a=>AuthenticationResult.success(a))
			case _ => FastFuture.successful(AuthenticationResult.failWithChallenge(HttpChallenge("HttpSig",None, Map[String,String]())))
		}
	}

	def keyIdtoUri(keyId: Rfc8941.SfString): Future[Uri] = FastFuture(Try(Uri(keyId.asciiStr)))

}

