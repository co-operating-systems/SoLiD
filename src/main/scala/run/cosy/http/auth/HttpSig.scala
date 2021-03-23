package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.model.headers.{GenericHttpCredentials, HttpChallenge, HttpCredentials, OAuth2BearerToken}
import akka.http.scaladsl.server.{Directive1, RequestContext}
import akka.http.scaladsl.server.Directives.{AsyncAuthenticator, AuthenticationResult, extractCredentials}
import akka.http.scaladsl.server.directives.{AuthenticationDirective, AuthenticationResult, Credentials}
import akka.http.scaladsl.server.directives.BasicDirectives.extractExecutionContext
import akka.http.scaladsl.util.FastFuture
import org.tomitribe.auth.signatures.{Algorithm, Signature, Signer, SigningAlgorithm, Verifier}

import java.security.PublicKey
import java.util
import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.matching.Regex

object HttpSig {
	
	trait Agent
	case class KeyAgent(keyId: Uri) extends Agent
	class Anonymous extends Agent
	
	case class PublicKeyAlgo(pubKey: PublicKey, algo: Algorithm)
	
	val URL: Regex = "<(.*)>".r

	/**
	 *  use with [[akka.http.scaladsl.server.directives.SecurityDirectives.authenticateOrRejectWithChallenge]]
 	 */
	def httpSigAuthN(req: HttpRequest)(fetch: Uri => Future[PublicKeyAlgo])(using
		 ec: ExecutionContext
	): Option[HttpCredentials] => Future[AuthenticationResult[Agent]] =
		case Some(c@GenericHttpCredentials("Signature",_,params)) =>
//			val sig = Try(Signature.fromString(c.toString()))
			def p(key: String) = params.get(key)
			(p("keyId"),p("algorithm"),p("headers"),p("signature")) match
				case (Some(URL(keyId)),Some("hs2019"),Some(headers),Some(sig)) =>
					import scala.jdk.CollectionConverters._
					fetch(keyId).map{ (pka: PublicKeyAlgo) =>
						val signature = new Signature(s"<$keyId>",
							SigningAlgorithm.HS2019,pka.algo,null,sig,List(headers.split("\\s+"):_*).asJava)
						println("Signature===="+signature.toString)
						val ver = new Verifier(pka.pubKey, signature)
						val headersMap = req.headers.foldRight(new util.HashMap[String,String]()){(h,m) => 
							m.put(h.lowercaseName,h.value); m}
						if ver.verify(req.method.value,s"<${req.uri}>",headersMap) then
							AuthenticationResult.success(KeyAgent(Uri(keyId)))
						else AuthenticationResult.failWithChallenge(HttpChallenge("httpsig",None)) 
					}
				case e => //todo: we need to return some more details on the failure 
					println("Failed because:"+e)
					FastFuture.successful(AuthenticationResult.failWithChallenge(HttpChallenge("httpsig",None)))	
		case None => //we return an anonymous agent 
			FastFuture.successful(AuthenticationResult.success(Anonymous()))
		case _ => //todo: find better way to deal with other Authorization attempts 
			FastFuture.successful(AuthenticationResult.failWithChallenge(HttpChallenge("httpsig",None)))
	

	def checkSignature(algorithm: Algorithm, pk: PublicKey, sign: List[String]): Unit = {
//		val sig: Signature = _
//		sig.
//		val sig = new Signature("some-key-1", SigningAlgorithm.HS2019, algorithm, null, null, sign.asJava)
//		val signer = new Signer(privateKey,sig)
//		val signed = signer.sign(method, uri, headers.asJava)
//		assertEquals(expected, signed.getSignature)
		???
	}

}
