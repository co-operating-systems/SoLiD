package run.cosy.http.headers

import run.cosy.http.headers.Rfc8941
import Rfc8941.{SfDict, SfInt, Token, IList => IL}
import Rfc8941.SyntaxHelper._
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.RawHeader
import run.cosy.ldp.testUtils.StringUtils._

import scala.collection.immutable.ListMap
import scala.language.existentials
import scala.util.{Success, Try}

class SignatureInputTest extends munit.FunSuite {
	import scala.language.implicitConversions
	import Rfc8941.Serialise.given

	val ex1 = """sig1=( "@request-target"  "host"   "date"    "cache-control" ); \
					|       keyid="</keys/key#k1>";  \
					|       alg="hs2019";   created=1402170695;    expires=1402170995""".rfc8792single

	val ex2 = """sig2=(   "host"   "date"  "cache-control" "@request-target"); \
					|       created=140217000;    expires=140220000; alg="hs2019";  \
					|       keyid="<https://alice.pdf/k/clef#>"""".rfc8792single

	// we don't recognise this one (yet), so it will be filtered out
	val ex3 = """sig3=("@request-target" "host" "date" "cache-control" "x-empty-header" \
					|     "x-example"); keyid="test-key-a"; alg="rsa-pss-sha512"; \
					|     created=1402170695; expires=1402170995""".rfc8792single

	val expected1 = IL(
		sf"@request-target", sf"host", sf"date", sf"cache-control")(
		Token("keyid")-> sf"</keys/key#k1>",
		Token("alg")-> sf"hs2019",
		Token("created")-> SfInt("1402170695"),
		Token("expires")-> SfInt("1402170995")
	)

	val expected2 = IL(
		sf"host", sf"date", sf"cache-control", sf"@request-target")(
		Token("created")-> SfInt("140217000"),
		Token("expires")-> SfInt("140220000"),
		Token("alg")-> sf"hs2019",
		Token("keyid")-> sf"<https://alice.pdf/k/clef#>"
	)

	test("one header") {
		val Success(tsi1) = `Signature-Input`.parse(ex1)
		val Some(sig1) = tsi1.get(Token("sig1"))
		assertEquals(sig1.il,expected1)

		val RawHeader(name,value) = `Signature-Input`(tsi1)
		assertEquals(name,"Signature-Input")
		val expectedHdr: SfDict = ListMap(Token("sig1")->expected1)
		assertEquals(value,expectedHdr.canon)

		RawHeader("Signature-Input",ex1) match
			case `Signature-Input`(sis) =>
				assertEquals(sis.si.size,1)
				assertEquals(sis.si.keys.head, Token("sig1"))
				assertEquals(sis.si.values.head, sig1)
				val sigIn: SigInput = sis.si.values.head
				assert(sigIn.headers.contains("cache-control"))
				assertEquals(sigIn.algo,sf"hs2019")
				assertEquals(sigIn.keyid,Uri("/keys/key#k1"))
				assertEquals(sigIn.created,Some(1402170695L))
				assertEquals(sigIn.expires,Some(1402170995L))
			case _ => fail
	}

	test("two headers") {
		val sigTxt = s"$ex1, $ex3,  $ex2"
		val Success(tsi1) = `Signature-Input`.parse(s"$ex1, $ex3,  $ex2")
		assertEquals(tsi1.si.size,2) // filtered out ex3
		val Some(sig1) = tsi1.get(Token("sig1"))
		val Some(sig2) = tsi1.get(Token("sig2"))
		assertEquals(sig1.il,expected1)
		assertEquals(sig2.il,expected2)

		val RawHeader(name,value) = `Signature-Input`( tsi1)
		assertEquals(name,"Signature-Input")
		val expectedHdr: SfDict = ListMap(Token("sig1")->expected1, Token("sig2")->expected2)
		assertEquals(value,expectedHdr.canon)

		RawHeader("Signature-Input",s"$ex1, $ex2, $ex3") match
			case `Signature-Input`(sis) =>
				assertEquals(sis.si.size,2)
				assertEquals(sis.si.keys.head, Token("sig1"))
				assertEquals(sis.si.keys.tail.head, Token("sig2"))
				assertEquals(sis.si.values.head, sig1)
				assertEquals(sis.si.values.tail.head, sig2)
				val sigIn: SigInput = sis.si.values.head
				assertEquals(sigIn.headers,Seq("@request-target","host","date","cache-control"))
				assertEquals(sigIn.algo,sf"hs2019")
				assertEquals(sigIn.keyid,Uri("/keys/key#k1"))
				assertEquals(sigIn.created,Some(1402170695L))
				assertEquals(sigIn.expires,Some(1402170995L))
				val sigIn2: SigInput = sis.si.values.tail.head
				assertEquals(sigIn2.headers,Seq( "host","date","cache-control","@request-target"))
				assertEquals(sigIn2.algo,sf"hs2019")
				assertEquals(sigIn2.keyid,Uri("https://alice.pdf/k/clef#"))
				assertEquals(sigIn2.created,Some(140217000L))
				assertEquals(sigIn2.expires,Some(140220000L))
			case _ => fail
	}

}
