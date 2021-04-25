package run.cosy.http.auth

import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.headers.{Authorization, HttpCredentials}
import akka.http.scaladsl.model.{HttpHeader, HttpMethod, HttpMethods, HttpRequest, Uri}
import akka.http.scaladsl.server
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.util.FastFuture
import junit.framework.Assert.fail
import org.tomitribe.auth.signatures.{Algorithm, PEM, Signature, Signatures, Signer, Verifier}
import run.cosy.http.auth.WebKeyidAgent
import run.cosy.ldp.testUtils.TmpDir

import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.security.PublicKey
import java.security.spec.AlgorithmParameterSpec
import scala.collection.immutable.HashMap
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.util.Success

object TestHttpSigRSAFn {
	val  privateKeyPem: String = "-----BEGIN RSA PRIVATE KEY-----\n" +
		"MIICXgIBAAKBgQDCFENGw33yGihy92pDjZQhl0C36rPJj+CvfSC8+q28hxA161QF\n" +
		"NUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6Z4UMR7EOcpfdUE9Hf3m/hs+F\n" +
		"UR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJwoYi+1hqp1fIekaxsyQIDAQAB\n" +
		"AoGBAJR8ZkCUvx5kzv+utdl7T5MnordT1TvoXXJGXK7ZZ+UuvMNUCdN2QPc4sBiA\n" +
		"QWvLw1cSKt5DsKZ8UETpYPy8pPYnnDEz2dDYiaew9+xEpubyeW2oH4Zx71wqBtOK\n" +
		"kqwrXa/pzdpiucRRjk6vE6YY7EBBs/g7uanVpGibOVAEsqH1AkEA7DkjVH28WDUg\n" +
		"f1nqvfn2Kj6CT7nIcE3jGJsZZ7zlZmBmHFDONMLUrXR/Zm3pR5m0tCmBqa5RK95u\n" +
		"412jt1dPIwJBANJT3v8pnkth48bQo/fKel6uEYyboRtA5/uHuHkZ6FQF7OUkGogc\n" +
		"mSJluOdc5t6hI1VsLn0QZEjQZMEOWr+wKSMCQQCC4kXJEsHAve77oP6HtG/IiEn7\n" +
		"kpyUXRNvFsDE0czpJJBvL/aRFUJxuRK91jhjC68sA7NsKMGg5OXb5I5Jj36xAkEA\n" +
		"gIT7aFOYBFwGgQAQkWNKLvySgKbAZRTeLBacpHMuQdl1DfdntvAyqpAZ0lY0RKmW\n" +
		"G6aFKaqQfOXKCyWoUiVknQJAXrlgySFci/2ueKlIE1QqIiLSZ8V8OlpFLRnb1pzI\n" +
		"7U1yQXnTAEFYM560yJlzUpOb1V4cScGd365tiSMvxLOvTA==\n" +
		"-----END RSA PRIVATE KEY-----\n";

	val publicKeyPem = "-----BEGIN PUBLIC KEY-----\n" +
		"MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDCFENGw33yGihy92pDjZQhl0C3\n" +
		"6rPJj+CvfSC8+q28hxA161QFNUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6\n" +
		"Z4UMR7EOcpfdUE9Hf3m/hs+FUR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJw\n" +
		"oYi+1hqp1fIekaxsyQIDAQAB\n" +
		"-----END PUBLIC KEY-----\n";

	lazy val privateKey = PEM.readPrivateKey(new ByteArrayInputStream(privateKeyPem.getBytes))
	lazy val publicKey: PublicKey = PEM.readPublicKey(new ByteArrayInputStream(publicKeyPem.getBytes))

}

class TestHttpSigRSAFn extends munit.FunSuite {
	import TestHttpSigRSAFn._

	import java.io.ByteArrayInputStream


//	val signature: Signature  = new Signature("key-alias", "hmac-sha256", null, "(request-target)");

	import org.tomitribe.auth.signatures.SigningAlgorithm
	import java.util

	val method = HttpMethods.POST
	val uri = Uri("/foo?param=value&pet=dog")

	val headers = Seq(
		"Host" -> "example.org",
		"Date" -> "Thu, 05 Jan 2012 21:31:40 GMT",
		"Content-Type" -> "application/json",
		"Digest" -> "SHA-256=X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE=",
		"Accept" -> "*/*",
		"Content-Length" -> "18"
	)
	def akkaHeaders(h: Seq[(String, String)]): Seq[HttpHeader] = h.map((h, v) =>
		HttpHeader.parse(h,v) match {
			case Ok(h,e) => h
			case ParsingResult.Error(e) => fail("error in our headers:"+e)
		})
	
	test("rsaSha512") {
		import org.tomitribe.auth.signatures.Algorithm
		val sha512rsaSig = JW2JCA.getSignerAndVerifier("SHA512withRSA").get
		val algorithm = Algorithm.RSA_SHA512

		assertSignature(sha512rsaSig, algorithm, "IItboA8OJgL8WSAnJa8MND04s9j7d" +
			"B6IJIBVpOGJph8Tmkc5yUAYjvO/UQUKytRBe5CSv2GLfTAmE" + 
			"7SuRgGGMwdQZubNJqRCiVPKBpuA47lXrKgC/wB0QAMkPHI6c" + 
			"PllBZRixmjZuU9mIbuLjXMHR+v/DZwOHT9k8x0ILUq2rKE=", 
			List("date"))

		assertSignature(sha512rsaSig, algorithm, "ggIa4bcI7q377gNoQ7qVYxTA4pEOl" +
			"xlFzRtiQV0SdPam4sK58SFO9EtzE0P1zVTymTnsSRChmFU2p" +
			"n+R9VzkAhQ+yEbTqzu+mgHc4P1L5IeeXQ5aAmGENfkRbm2vd" +
			"OZzP5j6ruB+SJXIlhnaum2lsuyytSS0m/GkWvFJVZFu33M=",
			List("(request-target)", "host", "date"))
	}

	test("rsaSha512 using httpSigAuthN") {
		val sha516rsaSig = JW2JCA.getSignerAndVerifier("SHA512withRSA").get
		val algorithm = Algorithm.RSA_SHA512
		testReq(sha516rsaSig, algorithm, List("date"))
		testReq(sha516rsaSig, algorithm, List("host", "date"))
		testReq(sha516rsaSig, algorithm, List("(request-target)", "host", "date"))
	}

	private def testReq(jcaSig: java.security.Signature, algorithm: Algorithm, sign: List[String]) = {
		import scala.concurrent.duration.*
		val sig = new Signature(s"<$uri>", SigningAlgorithm.HS2019, algorithm, null, null, sign.asJava)
		val signer = new Signer(privateKey,sig)
		val signature = signer.sign(method.value,s"<$uri>",headers.toMap.asJava)
		

		def verify(sig: String) =
			val authorization = "Authorization" -> sig
			val req: HttpRequest = HttpRequest(method, uri, akkaHeaders(headers :+ authorization))
			given ec: ExecutionContext = ExecutionContext.global
			val f: Future[server.Directives.AuthenticationResult[Agent]] =
				HttpSig.httpSigAuthN(req){ url =>
					assertEquals(url, uri)
					FastFuture.successful(SigVerificationData(publicKey,jcaSig)(url))
			}(req.header[Authorization].map(_.credentials))
			assertEquals(Await.result(f, 1.second), Right(WebKeyidAgent(uri)))

		verify(signature.toString)
	}

	@throws[Exception]
	def assertSignature(
		jcaSig: java.security.Signature,
		algorithm: Algorithm,
		expected: String,
		sign: List[String]
	): Unit = {
		val sig = new Signature("some-key-1", SigningAlgorithm.HS2019, algorithm , null, null, sign.asJava)
		val signer = new Signer(privateKey,sig)
		val signed: Signature = signer.sign(method.value, uri.toString(), headers.toMap.asJava)
		assertEquals(expected, signed.getSignature)

		//verify with Tomitribe
		val verifier = new Verifier(publicKey, signed)
		assert(verifier.verify(method.value,uri.toString(),headers.toMap.asJava))
	}
}
