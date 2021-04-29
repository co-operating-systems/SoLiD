package run.cosy.http.auth

import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.headers.{Authorization, HttpCredentials}
import akka.http.scaladsl.model.{HttpHeader, HttpMethod, HttpMethods, HttpRequest, Uri}
import akka.http.scaladsl.server
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.util.FastFuture
import junit.framework.Assert.fail
import run.cosy.http.auth.KeyIdAgent
import run.cosy.ldp.testUtils.TmpDir

import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.security.{PrivateKey, PublicKey}
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

	lazy val privateKey: PrivateKey = TestMessageSigningRFCFn.testKeyRSAPriv
	lazy val publicKey: PublicKey = TestMessageSigningRFCFn.testKeyRSApub

}

