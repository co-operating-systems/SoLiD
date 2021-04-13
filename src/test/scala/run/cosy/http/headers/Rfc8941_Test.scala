package run.cosy.http.headers

import cats.parse.Parser
import cats.parse.Parser.{Expectation, Fail}
import cats.data.NonEmptyList
import run.cosy.http.headers.Rfc8941
import Rfc8941.Parser.{dictMember, sfBinary, sfBoolean, sfDecimal, sfDictionary, sfInteger, sfList, sfNumber, sfString, sfToken}
import run.cosy.http.headers.Rfc8941.{IList, Param, Params, SfDict}

import java.util.Base64
import scala.collection.immutable.{ArraySeq, ListMap}

class Rfc8941_Test extends munit.FunSuite {

	val cafebabe = ArraySeq[Byte](113, -89, -34, 109, -90, -34)
	val cafedead = ArraySeq[Byte](113, -89, -34, 117, -26, -99)

	def R[T](value: T,remaining: String=""): Right[Parser.Error,(String, T)] = Right(remaining,value)
	def RA[T](value: T,remaining: String=""): Right[Parser.Error,T] = Right(value)
	def parseFail[T](result: Either[Parser.Error,(String, T)], msg: String="")(implicit loc: munit.Location): Unit =
		assert(result.isLeft,result)
	def parseFailAll[T](result: Either[Parser.Error,T], msg: String="")(implicit loc: munit.Location): Unit =
		assert(result.isLeft,result)

	import Rfc8941.Parser.*
	//
	// test Items
	//
	import Rfc8941.{DecStr, IntStr, PItem, Token}

	test("test sfBoolean") {
		assertEquals(sfBoolean.parse("?0"), R(false))
		assertEquals(sfBoolean.parse("?1"), R(true))
		parseFail(sfBoolean.parse("?x"))
		parseFail(sfBoolean.parse("000"))
	}

	test("test sfInteger") {
		assertEquals(sfInteger.parse("42"), R(IntStr("42")))
		assertEquals(sfInteger.parse("123456789012345"), R(IntStr("123456789012345")))
		assertEquals(sfInteger.parse("42.5"), R(IntStr("42"), ".5"))
		parseFailAll(sfInteger.parseAll("123hello"))
		parseFail(sfInteger.parse("hello"))
	}

	test("test sfDecimal") {
		assertEquals(sfDecimal.parse("42.0"), R(DecStr("42", "0")))
		assertEquals(sfDecimal.parse("123456789012.123"), R(DecStr("123456789012", "123")))
		assertEquals(sfDecimal.parse("42.5"), R(DecStr("42", "5")))
		parseFail(sfDecimal.parse("123456789012345.123"), "the dot is too far away")
		parseFail(sfDecimal.parse("123"), "there is not dot")
	}

	test("test sfNumber") {
		assertEquals(sfNumber.parse("42.0"), R(DecStr("42", "0")))
		assertEquals(sfNumber.parse("123456789012.123"), R(DecStr("123456789012", "123")))
		assertEquals(sfNumber.parse("-123456789012.123"), R(DecStr("-123456789012", "123")))
		assertEquals(sfNumber.parse("42.5"), R(DecStr("42", "5")))
		assertEquals(sfNumber.parse("-123456789012345.123"), R(IntStr("-123456789012345"), ".123"))
		assertEquals(sfNumber.parse("123"), R(IntStr("123")))
		assertEquals(sfNumber.parse("-123"), R(IntStr("-123")))
		parseFail(sfNumber.parse("a123"), "does not start with digit")
	}

	test("test sfString") {
		assertEquals(sfString.parse(""""42""""), R("42"))
		assertEquals(sfString.parse(""""123456789012345""""), R("123456789012345"))
		assertEquals(sfString.parse(""""a42b""""), R("a42b"))
		assertEquals(sfString.parse(""""a\"42\\b""""), R("""a"42\b"""))
		parseFail(sfString.parse(""""123456789012345"""), "no end quote")
		parseFail(sfString.parse(""""Bahnhofstraße"""), "no german here")
		parseFailAll(sfString.parseAll("""a"123hello""""), "letter before quote")
		parseFail(sfString.parse(""" "hello" """), "space before quote")
	}

	test("test sfToken") {
		assertEquals(sfToken.parse("foo123/456"), R(Token("foo123/456")))
		assertEquals(sfToken.parse("*logicomix:"), R(Token("*logicomix:")))
		assertEquals(sfToken.parse("*!#$%&'*+-.^_"), R(Token("*!#$%&'*+-.^_")))
		assertEquals(sfToken.parse("goodmorning"), R(Token("goodmorning")))
		parseFail(sfToken.parse("!hello"), "can't start with !")
		parseFailAll(sfToken.parseAll("#good morning"), "can't start with #")
		parseFailAll(sfToken.parseAll(" goodmorning"), "can't start with space")
		parseFailAll(sfToken.parseAll("good morning"), "can't contain space")
		parseFail(sfToken.parse(""" "hello" """), "space before quote")
	}

	test("test sfBinary") {
		assertEquals(sfBinary.parse(":cHJldGVuZCB0aGlzIGlzIGJpbmFyeSBjb250ZW50Lg==:"), R(ArraySeq.unsafeWrapArray(Base64.getDecoder.decode("cHJldGVuZCB0aGlzIGlzIGJpbmFyeSBjb250ZW50Lg=="))))
		assertEquals(sfBinary.parseAll(":cafebabe:"),RA(cafebabe))
		assertEquals(sfBinary.parseAll(":cafedead:"),RA(cafedead))
		parseFailAll(sfBinary.parseAll(" :cafedead:"), "can't start with space")
		parseFailAll(sfBinary.parseAll(":cHJldGVuZCB0aGlzIGlzIGJpbmFyeSBjb250ZW50Lg"), "must finish with colon")
		parseFailAll(sfBinary.parseAll(":cHJldGVuZCB0aGlz#IGlzIGJpbmFyeSBjb250ZW50Lg:"), "no hash in the middle")
	}
	import Rfc8941.{PItem => PI, Token, DecStr => Dec, IntStr}

	//
	// test Lists
	//

	test("test sfList") {
		assertEquals(
			sfList.parse("sugar, tea, rum"),
			R(List(PI(Token("sugar")), PI(Token("tea")), PI(Token("rum"))))
		)
		assertEquals(
			sfList.parse("sugar,tea,rum"),
			R(List(PI(Token("sugar")), PI(Token("tea")), PI(Token("rum"))))
		)
		assertEquals(
			sfList.parse("sugar, tea ,   rum"),
			R(List(PI(Token("sugar")), PI(Token("tea")), PI(Token("rum"))))
		)
		assertEquals(
			sfList.parse(""""sugar" , "tea",   "rum""""),
			R(List(PI("sugar"), PI("tea"), PI("rum")))
		)
		assertEquals(
			sfList.parse("123.45 , 34.33, 42, 56.789"),
			R(List(PI(Dec("123","45")), PI(Dec("34","33")), PI(IntStr("42")), PI(Dec("56","789"))))
		)
		assertEquals(
			sfList.parse("""123.450 , 034.33, 42, foo123/456 , ?0  ,  ?1, "rum", :cafebabe:"""),
			R(List(PI(Dec("123","450")), PI(Dec("034","33")), PI(IntStr("42")),
				PI(Token("foo123/456")), PI(false), PI(true), PI("rum"), PI(cafebabe)))
		)
		assertEquals(
			sfList.parse("""123.450 , 42, foo123/456 , ?0, "No/No", :cafebabe:"""),
			R(List(PI(Dec("123","450")), PI(IntStr("42")),
				PI(Token("foo123/456")), PI(false), PI("No/No"), PI(cafebabe)))
		)
		assertEquals(
			sfList.parse(
				"""1234.750;  n=4;f=3 , 42;magic="h2g2", foo123/456;lang=en ,
				  |   ?0;sleep=?1, "No/No", :cafebabe:;enc=unicode""".stripMargin.filter(_ != '\n').toString),
			R(List(PI(Dec("1234","750"),ListMap(Token("n")->IntStr("4"),Token("f")->IntStr("3"))),
				PI(IntStr("42"),ListMap(Token("magic")->"h2g2")),
				PI(Token("foo123/456"),ListMap(Token("lang")->Token("en"))),
				PI(false,ListMap(Token("sleep")->true)),
				PI("No/No"),
				PI(cafebabe,ListMap(Token("enc") -> Token("unicode")))))
		)
	}

	import Rfc8941.{IList=>IL,DictMember}

	//
	//Inner Lists
	//
	test("lists of innerList") {
		assertEquals(
			sfList.parse("""("foo" "bar"), ("baz"), ("bat" "one"), ()"""),
			R(List(
				IL(PI("foo"), PI("bar"))(),
				IL(PI("baz"))(),
				IL(PI("bat"), PI("one"))(),
				IL()()
			))
		)
		assertEquals(
			sfList.parse("""("foo"; a=1;b=2);lvl=5, ("bar" "baz");lvl=1"""),
			R(List(
				IL(List(PI("foo",ListMap(Token("a")->IntStr("1"),Token("b")->IntStr("2")))),ListMap(Token("lvl")->IntStr("5"))),
				IL(List(PI("bar"),PI("baz")),ListMap(Token("lvl")->IntStr("1")))
			))
		)
	}
	//
	// Dictionaries
	//
	test("dict-member") {
		assertEquals(
			dictMember.parse("""en="Applepie""""),
			R(DictMember(Token("en"),PI("Applepie")))
		)
	}

	test("sfDictionary"){
		assertEquals(
			sfDictionary.parse("""en="Applepie", da=:cafebabe:"""),
			R(ListMap(
				Token("en") -> PI("Applepie"),
				Token("da") -> PI(cafebabe)
			)))
		assertEquals(
			sfDictionary.parse("""a=?0, b, c; foo=bar"""),
			R(SfDict(
				Token("a") -> PI(false),
				Token("b") -> PI(true)(),
				Token("c") -> PI(true)(Token("foo") ->Token("bar"))
			)))
		assertEquals(
			sfDictionary.parse("""a=(1 2), b=3, c=4;aa=bb, d=(5 6);valid"""),
			R(SfDict(
				Token("a") -> IL(PI(IntStr("1")),PI(IntStr("2")))(),
				Token("b") -> PI(IntStr("3"))(),
				Token("c") -> PI(IntStr("4"))(Token("aa")->Token("bb")),
				Token("d") -> IL(PI(IntStr("5")),PI(IntStr("6")))(Token("valid")->true)
			)))
	}
	
	import run.cosy.ldp.testUtils.StringUtils._
	//examples are taken from https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03
	test("sfDictionary with Signing Http Messages headers") {
		//here we start playing with making the syntax easier to work with by using implicit conversions
		import scala.language.implicitConversions

		val `ex§4.1` = """sig1=("@request-target" "host" "date"   "cache-control" \
						|      "x-empty-header" "x-example"); keyid="test-key-a"; \
						|       alg="rsa-pss-sha512"; created=1402170695; expires=1402170995\
						|""".rfc8792single

		assertEquals(
			sfDictionary.parse(`ex§4.1`),
			R(ListMap(
				Token("sig1") -> IL(
					"@request-target","host","date","cache-control",
					"x-empty-header", "x-example"
				)(
					Token("keyid")->"test-key-a",
					Token("alg")->"rsa-pss-sha512",
					Token("created")->IntStr("1402170695"),
					Token("expires")->IntStr("1402170995")
				)
			))
		)

		val `ex§4.2`: String =
		"""sig1=:K2qGT5srn2OGbOIDzQ6kYT+ruaycnDAAUpKv+ePFfD0RAxn/1BUe\
		|     Zx/Kdrq32DrfakQ6bPsvB9aqZqognNT6be4olHROIkeV879RrsrObury8L9SCEibe\
		|     oHyqU/yCjphSmEdd7WD+zrchK57quskKwRefy2iEC5S2uAH0EPyOZKWlvbKmKu5q4\
		|     CaB8X/I5/+HLZLGvDiezqi6/7p2Gngf5hwZ0lSdy39vyNMaaAT0tKo6nuVw0S1MVg\
		|     1Q7MpWYZs0soHjttq0uLIA3DIbQfLiIvK6/l0BdWTU7+2uQj7lBkQAsFZHoA96ZZg\
		|     FquQrXRlmYOh+Hx5D9fJkXcXe5tmAg==:""".rfc8792single

		val `ex§4.2value`: ArraySeq[Byte]    =
		"""K2qGT5srn2OGbOIDzQ6kYT+ruaycnDAAUpKv+ePFfD0RAxn/1BUe\
		|  Zx/Kdrq32DrfakQ6bPsvB9aqZqognNT6be4olHROIkeV879RrsrObury8L9SCEibe\
		|  oHyqU/yCjphSmEdd7WD+zrchK57quskKwRefy2iEC5S2uAH0EPyOZKWlvbKmKu5q4\
		|  CaB8X/I5/+HLZLGvDiezqi6/7p2Gngf5hwZ0lSdy39vyNMaaAT0tKo6nuVw0S1MVg\
		|  1Q7MpWYZs0soHjttq0uLIA3DIbQfLiIvK6/l0BdWTU7+2uQj7lBkQAsFZHoA96ZZg\
		|  FquQrXRlmYOh+Hx5D9fJkXcXe5tmAg==""".rfc8792single.base64Decode

		assertEquals(
			sfDictionary.parse(`ex§4.2`),
			R(ListMap(Token("sig1") -> PI(`ex§4.2value`)))
		)
	}

	import Rfc8941.Serialise.{given,*}
	test("serialisation of Items") {
		assertEquals(true.canon, "?1")
		assertEquals(false.canon, "?0")
		assertEquals(Token("*ab/d").canon, "*ab/d")
		assertEquals(IntStr("234").canon, "234")
		assertEquals("hello".canon, """"hello"""")
		assertEquals(DecStr("1024","48").canon,"1024.48")
		assertEquals(cafebabe.canon,":cafebabe:")
		assertEquals(cafedead.canon,":cafedead:")
	}
	import Rfc8941.{Token=>Tk}
	test("serialisation of Parameterized Items") {
		assertEquals(Param("fun",true).canon,";fun")
		assertEquals(Params(Tk("fun")->true).canon,";fun")
		assertEquals(
			Params(Tk("foo")->true, Tk("bar")->IntStr("42")).canon,
			";foo;bar=42")
		assertEquals(
			Params(Tk("foo")->true, Tk("bar")->IntStr("42"),Tk("baz")->"hello").canon,
			""";foo;bar=42;baz="hello"""")
		assertEquals(
			Params(Tk("keyid")->cafebabe).canon,
			";keyid=:cafebabe:"
		)
		assertEquals(
			PItem(Tk("*foo"))(Tk("age")->IntStr("33")).canon,
			"*foo;age=33"
		)
		assertEquals(
			PItem(DecStr("99","999"))(Tk("discount")-> DecStr("0","2")).canon,
			"99.999;discount=0.2"
		)
		assertEquals(
			PItem(cafebabe)(Tk("enc")-> "utf8").canon,
			""":cafebabe:;enc="utf8""""
		)
	}

	test("serialisation of List") {
		import scala.language.implicitConversions
		//example from
		// https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-2.4.2.1
		// but whitespaces between attributes have been removed as per
		// issue: https://github.com/httpwg/http-extensions/issues/1456
		assertEquals(
			IList("@request-target", "host", "date","cache-control","x-empty-header", "x-example",
				PItem("x-dictionary")(Param("key",Tk("b"))),
				PItem("x-dictionary")(Param("key",Tk("a"))),
				PItem("x-list")(Param("prefix",IntStr("3"))))(
				Param("keyid","test-key-a"),
				Param("alg","rsa-pss-sha512"),
				Param("created",IntStr("1402170695")),
				Param("expires",IntStr("1402170995")),
			).canon,
			"""("@request-target" "host" "date" "cache-control" \
			  |   "x-empty-header" "x-example" "x-dictionary";key=b \
			  |   "x-dictionary";key=a "x-list";prefix=3);keyid="test-key-a";\
			  |   alg="rsa-pss-sha512";created=1402170695;expires=1402170995""".rfc8792single
		)
	}

	test("serialisation of sfDict") {
		import scala.language.implicitConversions
		assertEquals(
			SfDict(Token("key")->PItem(true)(Param("encoding",Token("utf8")))).canon,
			"key;encoding=utf8"
		)
		val `ex§4.1` = """sig1=("@request-target" "host" "date" "cache-control" \
							  |      "x-empty-header" "x-example");keyid="test-key-a";\
							  |       alg="rsa-pss-sha512";created=1402170695;expires=1402170995\
							  |""".rfc8792single

		assertEquals(
			SfDict(
				Token("sig1") -> IL(
					"@request-target","host","date","cache-control",
					"x-empty-header", "x-example"
				)(
					Token("keyid")->"test-key-a",
					Token("alg")->"rsa-pss-sha512",
					Token("created")->IntStr("1402170695"),
					Token("expires")->IntStr("1402170995")
				)
			).canon,
			`ex§4.1`
		)
	}
	
}
