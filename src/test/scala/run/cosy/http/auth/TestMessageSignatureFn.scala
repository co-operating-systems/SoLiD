package run.cosy.http.auth

import akka.http.scaladsl.model.headers.CacheDirectives._
import akka.http.scaladsl.model.{DateTime, HttpMessage, HttpRequest, MediaRanges, Uri}
import akka.http.scaladsl.model.headers._
import org.tomitribe.auth.signatures.{PEM, RSA}
import run.cosy.http.auth.MessageSignature._
import run.cosy.http.auth.TestHttpSigRSAFn.{privateKeyPem, publicKeyPem}
import run.cosy.http.headers.Rfc8941
import run.cosy.http.headers.SigInput
import Rfc8941.{IList, SfInt, Token}
import Rfc8941.Serialise
import Rfc8941.SyntaxHelper._

import scala.language.implicitConversions
import run.cosy.ldp.testUtils.StringUtils._

import java.io.ByteArrayInputStream
import java.security.{PublicKey, Signature => JSignature}
import java.time.ZoneId
import scala.concurrent.ExecutionContext
import scala.util.Success

class TestMessageSignatureFn extends munit.FunSuite {

	import TestHttpSigRSAFn.{publicKey,privateKey}
	lazy val sha512rsaSig: JSignature = JW2JCA.getSignerAndVerifier("SHA512withRSA").get

	val req1: HttpRequest = HttpRequest(uri=Uri("/hello/world"),headers=Seq(
		Accept(MediaRanges.`*/*`),
		Date(DateTime(2021,04,01)),
		ETag("686897696a7c876b7e"),
		`Cache-Control`(`max-age`(60), `must-revalidate`)
	))
	val req2 = HttpRequest(uri=Uri("/hello/world?who=me"),headers = req1.headers)

	val sigIn1 = SigInput(IList()(
		Token("keyid")-> sf"</keys/key#k1>",
		Token("created")-> SfInt("1402170695"),
		Token("expires")-> SfInt("1402170995")
	)).get
	val sigIn1Exp =
	""""@signature-params": ();keyid="</keys/key#k1>";created=1402170695;expires=1402170995"""

	val sigIn2 = SigInput(IList(sf"date")(sigIn1.il.params.toSeq*)).get
	val sigIn2Exp =
		""""date": Thu, 01 Apr 2021 00:00:00 GMT
		  |"@signature-params": ("date");keyid="</keys/key#k1>";created=1402170695;expires=1402170995""".stripMargin

	val sigIn3 = SigInput(IList(sf"date",sf"etag")(sigIn1.il.params.toSeq*)).get
	val sigIn3Exp =
		""""date": Thu, 01 Apr 2021 00:00:00 GMT
		  |"etag": "686897696a7c876b7e"
		  |"@signature-params": ("date" "etag");keyid="</keys/key#k1>";created=1402170695;expires=1402170995""".stripMargin
	val sigIn4 = SigInput(IList(sf"date",sf"etag",sf"cache-control")(sigIn1.il.params.toSeq*)).get
	val sigIn4Exp =""""date": Thu, 01 Apr 2021 00:00:00 GMT
						  |"etag": "686897696a7c876b7e"
						  |"cache-control": max-age=60, must-revalidate
						  |"@signature-params": ("date" "etag" "cache-control");keyid="</keys/key#k1>";created=1402170695;expires=1402170995""".stripMargin
	val sigIn5 = SigInput(IList(sf"@request-target",sf"etag",sf"cache-control")(sigIn1.il.params.toSeq*)).get
	val sigIn5Exp =
		""""@request-target": get /hello/world
		  |"etag": "686897696a7c876b7e"
		  |"cache-control": max-age=60, must-revalidate
		  |"@signature-params": ("@request-target" "etag" "cache-control");keyid="</keys/key#k1>";created=1402170695;expires=1402170995""".stripMargin
	val r2sigIn5Exp =
		""""@request-target": get /hello/world?who=me
		  |"etag": "686897696a7c876b7e"
		  |"cache-control": max-age=60, must-revalidate
		  |"@signature-params": ("@request-target" "etag" "cache-control");keyid="</keys/key#k1>";created=1402170695;expires=1402170995""".stripMargin

	test("signature string creation on a request") {
		assertEquals(req1.signingString(sigIn1),Success(sigIn1Exp))
		assertEquals(req1.signingString(sigIn2),Success(sigIn2Exp))
		assertEquals(req1.signingString(sigIn3),Success(sigIn3Exp))
		assertEquals(req1.signingString(sigIn4),Success(sigIn4Exp))
		assertEquals(req1.signingString(sigIn5),Success(sigIn5Exp))
		assertEquals(req2.signingString(sigIn5),Success(r2sigIn5Exp))
	}

	import akka.http.scaladsl.util.FastFuture
	import akka.http.scaladsl.model.headers.Authorization
	import java.time.Clock
	import scala.concurrent.Future

	def withSigInputTest(testName: String,
		msg: HttpMessage, sigName: String, sigIn: SigInput, sigData: SigningData, sigVerif: SigVerificationData
	)(using munit.Location): Unit = {
		test(testName) {
			val newReq: HttpMessage = msg.withSigInput(Rfc8941.Token(sigName),sigIn)
				.flatMap(fn=> fn(sigData)).toOption.get
			assertEquals(
				newReq.getHeader("Signature-Input").get.value(),
				RawHeader("Signature-Input",sigName +"="+sigIn.canon).value
			)

			// next: pretend we fetch the keyId over the web to verify the signature
			def fetchKeyId(uri: Uri) = FastFuture(Success(sigVerif))

			given ec: ExecutionContext = scala.concurrent.ExecutionContext.global
			given clock: Clock = Clock.fixed(java.time.Instant.ofEpochSecond(1402170700), java.time.ZoneOffset.UTC)

			//we create the credential object to test the signature
			val cred = GenericHttpCredentials("HttpSig", Map("name"->sigName))

			//note: we don't need to add `cred` to the message in an Authorization header. We can just use it
			//  to test our function.
			val fres = newReq.asInstanceOf[HttpRequest].signatureAuthN(fetchKeyId)(cred)
			import scala.concurrent.duration.given
			scala.concurrent.Await.ready(fres,2.seconds)
			assertEquals(
				fres.value,
				Some(Success(run.cosy.http.auth.HttpSig.KeyAgent(sigIn.keyid)))
			)
		}
	}

	val sigVerif = SigVerificationData(publicKey,sha512rsaSig)
	val sigdata= SigningData(privateKey,sha512rsaSig)

	withSigInputTest("req1 enhanced with empty SigInput",
		req1,"sig1",sigIn1,sigdata,sigVerif
	)

	withSigInputTest("req1 enhanced with (date) SigInput",
		req1,"sig1",sigIn2,sigdata,sigVerif
	)

	withSigInputTest("req1 enhanced with (date etag cache-control) SigInput",
		req1,"sig1",sigIn3,sigdata,sigVerif
	)


	withSigInputTest("req1 enhanced with (date etag cache-control) SigInput",
		req1,"sig1",sigIn4,sigdata,sigVerif
	)

	withSigInputTest("req1 enhanced with (@request-target etag cache-control) SigInput",
		req1,"sig1",sigIn5,sigdata,sigVerif
	)

	withSigInputTest("req2 enhanced with (@request-target etag cache-control) SigInput",
		req2,"sig1",sigIn5,sigdata,sigVerif
	)
	

}
