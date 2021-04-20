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
import java.security.{PublicKey, Signature=>JSignature}
import scala.util.Success

class TestMessageSignatureFn extends munit.FunSuite {

	import TestHttpSigRSAFn.{publicKey,privateKey}
	lazy val sha512rsaSig: JSignature = JW2JCA.getSignerAndVerifier("SHA512withRSA").get

	val req1: HttpMessage = HttpRequest(uri=Uri("/hello/world"),headers=Seq(
		Accept(MediaRanges.`*/*`),
		Date(DateTime(2021,04,01)),
		ETag("686897696a7c876b7e"),
		`Cache-Control`(`max-age`(60), `must-revalidate`)
	))
	val req2 = HttpRequest(uri=Uri("/hello/world?who=me"),headers = req1.headers)

	val sigIn1 = SigInput(IList()(
		Token("keyid")-> sf"</keys/key#k1>",
		Token("alg")-> sf"hs2019",
		Token("created")-> SfInt("1402170695"),
		Token("expires")-> SfInt("1402170995")
	)).get
	val sigIn1Exp =
	""""@signature-params": ();keyid="</keys/key#k1>";alg="hs2019";created=1402170695;expires=1402170995"""

	val sigIn2 = SigInput(IList(sf"date")(sigIn1.il.params.toSeq*)).get
	val sigIn2Exp =
		""""date": Thu, 01 Apr 2021 00:00:00 GMT
		  |"@signature-params": ("date");keyid="</keys/key#k1>";alg="hs2019";created=1402170695;expires=1402170995""".stripMargin

	val sigIn3 = SigInput(IList(sf"date",sf"etag")(sigIn1.il.params.toSeq*)).get
	val sigIn3Exp =
		""""date": Thu, 01 Apr 2021 00:00:00 GMT
		  |"etag": "686897696a7c876b7e"
		  |"@signature-params": ("date" "etag");keyid="</keys/key#k1>";alg="hs2019";created=1402170695;expires=1402170995""".stripMargin
	val sigIn4 = SigInput(IList(sf"date",sf"etag",sf"cache-control")(sigIn1.il.params.toSeq*)).get
	val sigIn4Exp =""""date": Thu, 01 Apr 2021 00:00:00 GMT
						  |"etag": "686897696a7c876b7e"
						  |"cache-control": max-age=60, must-revalidate
						  |"@signature-params": ("date" "etag" "cache-control");keyid="</keys/key#k1>";alg="hs2019";created=1402170695;expires=1402170995""".stripMargin
	val sigIn5 = SigInput(IList(sf"@request-target",sf"etag",sf"cache-control")(sigIn1.il.params.toSeq*)).get
	val sigIn5Exp =
		""""@request-target": get /hello/world
		  |"etag": "686897696a7c876b7e"
		  |"cache-control": max-age=60, must-revalidate
		  |"@signature-params": ("@request-target" "etag" "cache-control");keyid="</keys/key#k1>";alg="hs2019";created=1402170695;expires=1402170995""".stripMargin
	val r2sigIn5Exp =
		""""@request-target": get /hello/world?who=me
		  |"etag": "686897696a7c876b7e"
		  |"cache-control": max-age=60, must-revalidate
		  |"@signature-params": ("@request-target" "etag" "cache-control");keyid="</keys/key#k1>";alg="hs2019";created=1402170695;expires=1402170995""".stripMargin



	test("signature string creation on a request") {
		assertEquals(req1.signingString(sigIn1),Success(sigIn1Exp))
		assertEquals(req1.signingString(sigIn2),Success(sigIn2Exp))
		assertEquals(req1.signingString(sigIn3),Success(sigIn3Exp))
		assertEquals(req1.signingString(sigIn4),Success(sigIn4Exp))
		assertEquals(req1.signingString(sigIn5),Success(sigIn5Exp))
		assertEquals(req2.signingString(sigIn5),Success(r2sigIn5Exp))
	}

	def withSigInputTest(testName: String,
		msg: HttpMessage, sigName: String, sigIn: SigInput, sigData: SigningData, expectedSignature: String
	)(using munit.Location): Unit = {
		test(testName) {
			val newReq: HttpMessage = msg.withSigInput(Rfc8941.Token(sigName),sigIn)
				.flatMap(fn=> fn(sigData)).toOption.get
			assertEquals(newReq.getHeader("Signature-Input").get.value(),RawHeader("Signature-Input",sigName +"="+sigIn.canon).value)
			assertEquals(newReq.getHeader("Signature").get.value(),RawHeader("Signature",expectedSignature).value)
		}
	}
	val sigdata= SigningData(privateKey,sha512rsaSig)

	withSigInputTest("req1 enhanced with empty SigInput",
		req1,"sig1",sigIn1,sigdata,
		"sig1=:OrgbXLzKn48mbuWzFzawxMq/uvnzDkcDwAIlozUdn4vAb/OKzV40c8R5s3Bz7DpX9Og6C09neFkU1s1ri6vP8QX3oGcbu5" +
			"REDiBjtZrkwodlYAz8HdKYBJJ7MeJ6baFrVwXns26CFN8Bt3QVcIGFuyvfuzkfOlpI/reTh8kzBQY=:"
	)

	withSigInputTest("req1 enhanced with (date) SigInput",
		req1,"sig1",sigIn2,sigdata,
		"sig1=:i+NnbNAt5eUcXV0CuIK2q41hZdfYW+l0ERVJWAV2ECLCE/z39HL+S1xr6Yuny++4ujGHQCMAFvaAIokJFxSOzmNz7J/s9" +
			"Xh4fluIPSGhsbqWvqAJqBGdXbVjmMPu9csD2OqS2E2ml6rfoSsxZOi0DxGesOTZJXdLtL7HhF+ubpY=:"
	)

	withSigInputTest("req1 enhanced with (date etag cache-control) SigInput",
		req1,"sig1",sigIn3,sigdata,
		"sig1=:Kzv7FShPMC0+8Y0jPIMngPCTjhwF9jjLM2oRRiQfRoKtjQUmxXrcb0lw8akapCqgrur/vXnE8I/hwBeGmtAqE9eHXnsX" +
			"d6/SBI9AGtPbnWMI6G0q2gLe6Uro3wqlhR+LLq+d0VnSkZtu8wMVz7Z4G1iLbfkPUIOV1SY1iFjxC7I=:"
	)


	withSigInputTest("req1 enhanced with (date etag cache-control) SigInput",
		req1,"sig1",sigIn4,sigdata,
		"sig1=:IEvS/0b037MdVH2IYnfYeeYTQAaZqr++EoXEzslm+d2R1r41ApZlLQdC6QF+CfD6rlhswjbHrrjNBe93oFX/J2QM" +
			"LD7tOnbrZBdnIT76jPUP5wWde17m20gwph/4KrLY5O/rgGJbIexpC4rsd9O17fCEAkjP6POeeFpZV01+GUs=:"
	)

	withSigInputTest("req1 enhanced with (@request-target etag cache-control) SigInput",
		req1,"sig1",sigIn5,sigdata,
		"sig1=:OZttJHDlmjoksjcK2XQqSssIQcD06Bq/FYBqLbUGRDHaQzE2/B2WdpGocCFptCHKx40TdxblgrLXql" +
			"Un17mM4DyvUCPHBw1ysAFglrjjAcguvsHTFITezeSqcQKhEosjOHQ1slJEdTragnrCLsbyPz0lYpMP6PluCffgKgVWZEw=:"
	)

	withSigInputTest("req2 enhanced with (@request-target etag cache-control) SigInput",
		req2,"sig1",sigIn5,sigdata,
		"sig1=:iYjFxMDdkOYqNasR+OV3Qlgb2Mn0P3cTszUOO1v4saEwajqtTRuZOgiSuOmjbokT0KHcb0mNpX7dCMdcsIXAgSX9N" +
			"YbuJZQZJG9apUAafrZKqqSNETzEIz2zW8X5LIPN/Ovv2KCe8gz1HlGAMFJCNk4rRHbSdUBZ0iCQgGvOgkg=:"
	)
	

}
