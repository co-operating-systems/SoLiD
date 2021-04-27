package run.cosy.http.auth

import akka.http.scaladsl.model.{DateTime, HttpMethods, HttpRequest, HttpResponse, MediaRange, MediaRanges, StatusCodes, Uri}
import akka.http.scaladsl.model.headers.{`WWW-Authenticate`, Accept, Authorization, Date, GenericHttpCredentials, HttpChallenge, Link, LinkParam, LinkParams, LinkValue}
import HttpMethods._
import akka.http.scaladsl.model.Uri.Host
import run.cosy.http.RDFMediaTypes._
import run.cosy.http.headers.{Rfc8941, SigInput}
import run.cosy.ldp.testUtils.StringUtils._
import run.cosy.http.auth.MessageSignature._
import run.cosy.http.headers.Rfc8941._
import run.cosy.http.headers.Rfc8941.SyntaxHelper._

import scala.language.implicitConversions
import java.time.{Clock, Month, ZoneOffset}
import java.util.Calendar
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
 * Tests for the [[https://github.com/solid/authentication-panel/blob/main/proposals/HttpSignature.md HttpSig spec proposal]]
 * This will help make sure the examples are well formed.
 **/
class TestHttpSigSpecFn extends munit.FunSuite {
	 // we use the same keys as Signing Http Messages
	import TestMessageSigningRFCFn._

	val exMediaRanges: Seq[MediaRange] =
		Seq(`application/ld+json`.withQValue(0.9),MediaRanges.`*/*`.withQValue(0.8))

	val resp1Ex = HttpResponse(StatusCodes.Unauthorized,
		Seq(`WWW-Authenticate`(HttpChallenge("HttpSig","/comments/",Map())),
			akka.http.scaladsl.model.headers.Host("alice.name"),
			Date(DateTime(2021,04,01,00,01,03)),
			Link(LinkValue(Uri("http://www.w3.org/ns/ldp#BasicContainer"),LinkParams.rel("type"))),
			Link(LinkValue(Uri("http://www.w3.org/ns/ldp#Resource"),LinkParams.rel("type"))),
			Link(LinkValue(Uri("/comments/.acl"),LinkParams.rel("acl")))
		)
	)

	val req1Ex = HttpRequest(GET, Uri("/comments/"), Seq(
		Authorization(GenericHttpCredentials("HttpSig",Map("proof"->"sig1","cred"->"https://age.com/cred/xyz#"))),
		Accept(`text/turtle`.withQValue(1.0), exMediaRanges*)
		))

	given ec: ExecutionContext = scala.concurrent.ExecutionContext.global
	given clock: Clock = Clock.fixed(java.time.Instant.ofEpochSecond(16188845000), java.time.ZoneOffset.UTC)
	import run.cosy.http.headers.akka.{given,_}
	val t = java.time.LocalDateTime.of(2021, Month.APRIL, 01, 8, 30)
	val t2 = java.time.LocalDateTime.of(2021, Month.APRIL, 01, 23, 45)
	val pubkeyPEM = """-----BEGIN RSA PUBLIC KEY-----
							|MIIBCgKCAQEAhAKYdtoeoy8zcAcR874L8cnZxKzAGwd7v36APp7Pv6Q2jdsPBRrw
							|WEBnez6d0UDKDwGbc6nxfEXAy5mbhgajzrw3MOEt8uA5txSKobBpKDeBLOsdJKFq
							|MGmXCQvEG7YemcxDTRPxAleIAgYYRjTSd/QBwVW9OwNFhekro3RtlinV0a75jfZg
							|kne/YiktSvLG34lw2zqXBDTC5NHROUqGTlML4PlNZS5Ri2U4aCNx2rUPRcKIlE0P
							|uKxI4T+HIaFpv8+rdV6eUgOrB2xeI1dSFFn/nnv5OoZJEIB+VmuKn3DCUcCZSFlQ
							|PSXSfBDiUGhwOw76WuSSsf1D4b/vLoJ10wIDAQAB
							|-----END RSA PUBLIC KEY-----""".stripMargin
	test("intro example") {
		val Some(si) = SigInput(IList(`@request-target`.sf, authorization.sf)(
			Token("keyid") -> sf"/keys/alice#",
			Token("created") -> SfInt(t.toInstant(ZoneOffset.UTC).toEpochMilli/1000),
			Token("expires") -> SfInt(t2.toInstant(ZoneOffset.UTC).toEpochMilli/1000)
		))
		val Success(req1signingStr) = req1Ex.signingString(si)
		val Success(req1signed) = req1Ex.withSigInput(Rfc8941.Token("sig1"),si).flatMap(_(`test-key-rsa-pss-sigdata`))
		println("----401 response----")
		println(resp1Ex.documented.toRfc8792single())
		println("----partial request to be signed---")
		println(req1Ex.documented.toRfc8792single())
		println("---signing string----")
		println(req1signingStr.toRfc8792single())
		println("----signed request----")
		println(req1signed.documented.toRfc8792single())
		import com.nimbusds.jose.jwk.JWK
		val jwk = JWK.parseFromPEMEncodedObjects(pubkeyPEM)
		println("----json public key---")
		println(jwk.toJSONString)
		//println(sigReq.get.documented)
	}
}


