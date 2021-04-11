package run.cosy.http.headers

import cats.parse.Parser
import cats.parse.Parser.{Expectation, Fail}
import cats.data.NonEmptyList

import java.util.Base64
import scala.collection.immutable.{ArraySeq, ListMap}

class Rfc8941_Test extends munit.FunSuite {
	import Rfc8941._

	val cafebabe = ArraySeq[Byte](113, -89, -34, 109, -90, -34)
	val cafedead = ArraySeq[Byte](113, -89, -34, 117, -26, -99)

	def R[T](value: T,remaining: String=""): Right[Parser.Error,(String, T)] = Right(remaining,value)
	def RA[T](value: T,remaining: String=""): Right[Parser.Error,T] = Right(value)
	def parseFail[T](result: Either[Parser.Error,(String, T)], msg: String="")(implicit loc: munit.Location): Unit =
		assert(result.isLeft,result)
	def parseFailAll[T](result: Either[Parser.Error,T], msg: String="")(implicit loc: munit.Location): Unit =
		assert(result.isLeft,result)
	{
		//
		// test Items
		//
		import run.cosy.http.headers.Rfc8941Types.{DecStr, IntStr, PItem, Token}

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
	}
	{
		import run.cosy.http.headers.Rfc8941Types.{PItem => PI, Token => Tk, DecStr => Dec, IntStr}

		//
		// test Lists
		//

		test("test sfList") {
			assertEquals(
				sfList.parse("sugar, tea, rum"),
				R(List(PI(Tk("sugar")), PI(Tk("tea")), PI(Tk("rum"))))
			)
			assertEquals(
				sfList.parse("sugar,tea,rum"),
				R(List(PI(Tk("sugar")), PI(Tk("tea")), PI(Tk("rum"))))
			)
			assertEquals(
				sfList.parse("sugar, tea ,   rum"),
				R(List(PI(Tk("sugar")), PI(Tk("tea")), PI(Tk("rum"))))
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
					PI(Tk("foo123/456")), PI(false), PI(true), PI("rum"), PI(cafebabe)))
			)
			assertEquals(
				sfList.parse("""123.450 , 42, foo123/456 , ?0, "No/No", :cafebabe:"""),
				R(List(PI(Dec("123","450")), PI(IntStr("42")),
					PI(Tk("foo123/456")), PI(false), PI("No/No"), PI(cafebabe)))
			)
			assertEquals(
				sfList.parse(
					"""1234.750;  n=4;f=3 , 42;magic="h2g2", foo123/456;lang=en ,
					  |   ?0;sleep=?1, "No/No", :cafebabe:;enc=unicode""".stripMargin.filter(_ != '\n').toString),
				R(List(PI(Dec("1234","750"),ListMap("n"->IntStr("4"),"f"->IntStr("3"))),
					PI(IntStr("42"),ListMap("magic"->"h2g2")),
					PI(Tk("foo123/456"),ListMap("lang"->Tk("en"))),
					PI(false,ListMap("sleep"->true)),

					PI("No/No"),
					PI(cafebabe,ListMap("enc" -> Tk("unicode")))))
			)
		}

		import run.cosy.http.headers.Rfc8941Types.{IList=>IL,DictMember}

		//
		//Inner Lists
		//
		test("lists of innerList") {
			assertEquals(
				sfList.parse("""("foo" "bar"), ("baz"), ("bat" "one"), ()"""),
				R(List(
					IL(List(PI("foo"), PI("bar"))),
					IL(List(PI("baz"))),
					IL(List(PI("bat"), PI("one"))),
					IL(List())
				))
			)
			assertEquals(
				sfList.parse("""("foo"; a=1;b=2);lvl=5, ("bar" "baz");lvl=1"""),
				R(List(
					IL(List(PI("foo",ListMap("a"->IntStr("1"),"b"->IntStr("2")))),ListMap("lvl"->IntStr("5"))),
					IL(List(PI("bar"),PI("baz")),ListMap("lvl"->IntStr("1")))
				))
			)
		}
		//
		// Dictionaries
		//
		test("dict-member") {
			assertEquals(
				dictMember.parse("""en="Applepie""""),
				R(DictMember("en",PI("Applepie")))
			)
		}

		test("sfDictionary"){
			assertEquals(
				sfDictionary.parse("""en="Applepie", da=:cafebabe:"""),
				R(ListMap(
					"en" -> PI("Applepie"),
					"da" -> PI(cafebabe)
				)))
			assertEquals(
				sfDictionary.parse("""a=?0, b, c; foo=bar"""),
				R(ListMap(
					"a" -> PI(false),
					"b" -> PI(()),
					"c" -> PI((),ListMap("foo"->Tk("bar")))
				)))
			assertEquals(
				sfDictionary.parse("""a=(1 2), b=3, c=4;aa=bb, d=(5 6);valid"""),
				R(ListMap(
					"a" -> IL(List(PI(IntStr("1")),PI(IntStr("2")))),
					"b" -> PI(IntStr("3")),
					"c" -> PI(IntStr("4"),ListMap("aa"->Tk("bb"))),
					"d" -> IL(List(PI(IntStr("5")),PI(IntStr("6"))),ListMap("valid"->()))
				)))
		}

	}
}
