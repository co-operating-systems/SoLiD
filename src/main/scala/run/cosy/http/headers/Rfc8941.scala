package run.cosy.http.headers

import akka.http.scaladsl.model.ParsingException
import cats.parse.Parser
import run.cosy.http.WebException
import run.cosy.http.headers.Rfc8941.Serialise.Serialise

import java.math.{MathContext, RoundingMode}
import java.nio.charset.{Charset, StandardCharsets}
import java.util.Base64
import scala.collection.immutable.{ArraySeq, ListMap}
import scala.reflect.TypeTest
import scala.util.{Failure, Success, Try}


object Rfc8941 {

	//
	//types used by RFC8941
	//

	case class NumberOutOfBoundsException(num: Number) extends WebException

	/** SFInt's cover a subspace of Java Longs.
	 * An Opaque type would not do, as these need to be pattern matched.
	 * Only the object constructor can build these */
	final case class SfInt private(val long: Long) extends AnyVal

	object SfInt:
		val MAX_VALUE: Long = 999_999_999_999_999
		val MIN_VALUE: Long = -MAX_VALUE

		/** We throw a stackfree exception. Calling code can wrap in Try. */
		@throws[NumberOutOfBoundsException]
		def apply(long: Long): SfInt =
			if long <= MAX_VALUE && long >= MIN_VALUE then new SfInt(long)
			else throw NumberOutOfBoundsException(long)

		@throws[NumberOutOfBoundsException]
		@throws[NumberFormatException]
		def apply(longStr: String): SfInt = apply(longStr.toLong)

		//no need to check bounds if parsed by parser below
		private[Rfc8941] def unsafeParsed(longStr: String): SfInt = new SfInt(longStr.toLong)
	end SfInt

	/* https://www.rfc-editor.org/rfc/rfc8941.html#ser-decimal */
	final case class SfDec private(val double: Double) extends AnyVal

	object SfDec :
		val MAX_VALUE: Double = 999_999_999_999.999
		val MIN_VALUE: Double = -MAX_VALUE
		val mathContext = new MathContext(3,RoundingMode.HALF_EVEN)

		@throws[NumberOutOfBoundsException]
		def apply(d: Double): SfDec =
			if d <= MAX_VALUE && d >= MIN_VALUE then {
				new SfDec(BigDecimal(d).setScale(3, BigDecimal.RoundingMode.HALF_EVEN).doubleValue)
			} else throw NumberOutOfBoundsException(d)

		@throws[NumberFormatException]
		def apply(sfDecStr: String): SfDec = Parser.sfDecimal.parseAll(sfDecStr) match {
			case Left(err) => throw new NumberFormatException(err.toString)
			case Right(num) => num
		}

		//no need to check bounds if parsed by parser below
		private[Rfc8941] def unsafeParsed(int: String, fract: String): SfDec =
			new SfDec(BigDecimal(int+"."+fract).setScale(3, BigDecimal.RoundingMode.HALF_EVEN).doubleValue)
	end SfDec

	/**
	 * class has to be abstract to remove the `copy` operation which would allow objects
	 * outside this package to create illegal values.
	 **/
	final case class SfString private(val asciiStr: String) extends AnyVal:
		/** the string formatted for inclusion in Rfc8941 output with surrounding "..." and escaped \ and " */
		def formattedString: String = {
			import SfString.bs
			val sb = new StringBuilder
			sb.append('"')
			for (c <- asciiStr) {
				if (c == '\\') sb.append(bs).append(bs)
				else if (c == '"') sb.append(bs).append('"')
				else sb.append(c)
			}
			sb.append('"')
			sb.toString()
		}

	object SfString:
		def isAsciiChar(c: Int): Boolean = (c > 0x1f) && (c < 0x7f)
		val bs = '\\'
		@throws[IllegalArgumentException]
		def apply(str: String): SfString =
			if str.forall(isAsciiChar) then new SfString(str)
			else throw new IllegalArgumentException(s"$str<<< contains non ascii chars ")

		private[Rfc8941] def unsafeParsed(asciiStr: List[Char]): SfString = new SfString(asciiStr.mkString)
	end SfString

	// class is abstract to remove copy operation
	final case class Token private(val t: String) extends AnyVal

	object Token:
		@throws[ParsingException]
		def apply(t: String): Token = Parser.sfToken.parseAll(t) match
			case Right(value) => value
			case Left(err) => throw ParsingException(s"error paring token $t",s"failed at offset ${err.failedAtOffset}")

		private[Rfc8941] def unsafeParsed(name: String) = new Token(name)
	end Token

	sealed trait Parameterized {
		def params: Params
	}

	/**
	 * see [[https://www.rfc-editor.org/rfc/rfc8941.html#section-3.3 §3.3 Items]] of RFC8941.
	 */
	type Item = SfInt | SfDec | SfString | Token | ArraySeq[Byte] | Boolean
	type Param = (Token, Item)
	type Params = ListMap[Token, Item]
	type SfList = List[Parameterized]
	type SfDict = ListMap[Token, Parameterized]

	def Param(tk: String, i: Item): Param = (Token(tk),i)
	def Params(ps: Param*): Params = ListMap(ps*)
	def SfDict(entries: (Token,Parameterized)*) = ListMap(entries*)
	/**
	 * dict-member    = member-key ( parameters / ( "=" member-value ))
	 * member-value   = sf-item / inner-list
	 *
	 * @param key    member-key
	 * @param values if InnerList with an empty list, then we have "parameters", else we have an inner list
	 */
	final
	case class DictMember(key: Token, values: Parameterized)

	/** Parameterized Item */
	final
	case class PItem(item: Item, params: Params) extends Parameterized

	object PItem:
		def apply(item: Item): PItem = new PItem(item,ListMap())
		def apply(item: Item)(params: Param*): PItem = new PItem(item,ListMap(params*))

	/** Inner List */
	final case class IList(items: List[PItem], params: Params) extends Parameterized

	object IList:
		def apply(items: PItem*)(params: Param*): IList = new IList(items.toList,ListMap(params*))

	implicit val token2PI: Conversion[Item,PItem] = (i: Item) => PItem(i)
	private def paramConversion(paras: Param*): Params = ListMap(paras*)
	implicit val paramConv: Conversion[Seq[Param],Params] = paramConversion

	object SyntaxHelper:
		extension (sc: StringContext)
			def sf(args: Any*): SfString = {
				val strings = sc.parts.iterator
				val expressions = args.iterator
				var buf = new StringBuilder(strings.next())
				while(strings.hasNext) {
					buf.append(expressions.next())
					buf.append(strings.next())
				}
				SfString( buf.toString())
			}


	object Parser:
		import cats.parse.{Rfc5234=>R5234}
		import cats.parse.Numbers.{nonNegativeIntString, signedIntString}
		import cats.parse.{Parser => P, Parser0 => P0}
		import run.cosy.http.headers.Rfc7230.ows

		private val `*` = P.charIn('*')
		private val `:` = P.char(':')
		private val `?` = P.char('?')
		private val `.` = P.char('.')
		private val bs = '\\'
		private val minus = P.char('-')
		private val `\\`: P[Unit] = P.char(bs)

		val boolean: P[Boolean] = R5234.bit.map(_ == '1')
		val sfBoolean: P[Boolean] = (`?` *> boolean)
		val sfInteger: P[SfInt] = (minus.?.with1 ~ R5234.digit.rep(1, 15)).string.map(s=> Rfc8941.SfInt.unsafeParsed(s))
		val decFraction: P[String] = R5234.digit.rep(1, 3).string
		val signedDecIntegral: P[String] = (minus.?.with1 ~ R5234.digit.rep(1, 12)).map { case (min, i) =>
			min.map(_ => "-").getOrElse("") + i.toList.mkString
		}
		val sfDecimal: P[SfDec] =
			(signedDecIntegral ~ (`.` *> decFraction)).map { case (dec, frac) =>
				SfDec.unsafeParsed(dec, frac.toList.mkString) //todo: keep the non-empty list?
			}

		// first we have to check for decimals, then for integers, or there is the risk that the `.` won't be noticed
		// todo: optimisation would remove backtracking here
		val sfNumber: P[SfDec|SfInt] = (sfDecimal.backtrack.orElse(sfInteger))

		/**
		 * unescaped      =  SP / %x21 / %x23-5B / %x5D-7E
		 * note: this is similar to [[run.cosy.http.headers.Rfc5234]] except
		 * except that tabs are not allowed an anything above the ascii char set.
		 * */
		val unescaped: P[Char] =
			P.charIn(' ', 0x21.toChar)
				.orElse(P.charIn(0x23.toChar to 0x5b.toChar))
				.orElse(P.charIn(0x5d.toChar to 0x7e.toChar))
		val escaped: P[Char] = (`\\` *> (P.charIn(bs, '"')))
		val sfString: P[SfString] = (R5234.dquote *> (unescaped | escaped).rep0 <* R5234.dquote)
			.map(chars => SfString.unsafeParsed(chars))
		val sfToken: P[Token] = ((R5234.alpha | P.charIn('*')) ~ (Rfc7230.tchar | P.charIn(':', '/')).rep0)
			.map { (c, lc) => Token.unsafeParsed((c :: lc).mkString) }
		val base64: P[Char] = (R5234.alpha | R5234.digit | P.charIn('+', '/', '='))
		val sfBinary: P[ArraySeq[Byte]] = (`:` *> base64.rep0 <* `:`).map { chars =>
			ArraySeq.unsafeWrapArray(Base64.getDecoder.decode(chars.mkString))
		}
		val bareItem: P[Item] = P.oneOf(sfNumber :: sfString :: sfToken :: sfBinary :: sfBoolean :: Nil)
		val lcalpha: P[Char] = P.charIn(0x61.toChar to 0x7a.toChar) | P.charIn('a' to 'z')

		val key: P[Token] = ((lcalpha | `*`) ~ (lcalpha | R5234.digit | P.charIn('_', '-', '.', '*')).rep0)
			.map((c, lc) => Token((c :: lc).mkString))

		val parameter: P[Param] =
			(key ~ (P.char('=') *> bareItem).orElse(P.pure(true)))

		//note: parameters always returns an answer (the empty list) as everything can have parameters
		//todo: this is not exeactly how it is specified, so check here if something goes wrong
		val parameters: P0[Params] =
		(P.char(';') *> ows *> parameter).rep0.orElse(P.pure(List())).map { list =>
			ListMap.from[Token, Item](list.iterator)
		}

		val sfItem: P[PItem] = (bareItem ~ parameters).map((item, params) => PItem(item, params))

		val innerList: P[IList] = {
			import R5234.sp
			(((P.char('(') ~ sp.rep0) *> ((sfItem ~ (sp.rep(1) *> sfItem).rep0 <* sp.rep0).?) <* P.char(')')) ~ parameters)
				.map {
					case (Some(pi, lpi), params) => IList(pi :: lpi, params)
					case (None, params) => IList(List(), params)
				}
		}
		val listMember: P[Parameterized] = (sfItem | innerList)

		val sfList: P[SfList] =
			(listMember ~ ((ows *> P.char(',') *> ows).void.with1 *> listMember).rep0).map((p, lp) => p :: lp)

		val memberValue: P[Parameterized] = (sfItem | innerList)
		//note: we have to go with parsing `=` first as parameters always returns an answer.
		val dictMember: P[DictMember] = (key ~ (P.char('=') *> memberValue).eitherOr(parameters))
			.map {
				case (k, Left(parameters)) => DictMember(k, PItem(true,parameters))
				case (k, Right(parameterized)) => DictMember(k, parameterized)
			}
		val sfDictionary: P[SfDict] =
			(dictMember ~ ((ows *> P.char(',') *> ows).with1 *> dictMember).rep0).map((dm, list) =>
				val x: List[DictMember] = dm :: list
					//todo: avoid this tupling
					ListMap.from(x.map((d: DictMember) => Tuple.fromProductTyped(d)))
		)
	end Parser

	/**
	 *
 	 * Serialisation implementations for the RFC8941 types as defined in
	 * [[https://www.rfc-editor.org/rfc/rfc8941.html#section-4.1 §4.1 Serializing Structured Fields]]
	 * written as a type class so thqt it can easily be extended to give the result with non RFC8941 headers
	 * and work with different frameworks.
	 *
	 * todo: it may be that scalaz's Show class that uses a Cord
	 **/
	object Serialise:

		trait Serialise[-T]:
			extension (o: T)
			   //may be better if encoded directly to a byte string
				def canon: String

		given itemSer: Serialise[Item] with
			extension (o: Item)
				def canon: String = o match
					case i: SfInt => i.long.toString
					//todo: https://www.rfc-editor.org/rfc/rfc8941.html#ser-decimal
					case d: SfDec => d.double.toString
					case s: SfString => s.formattedString
					case tk: Token => tk.t
					case as: ArraySeq[Byte] => ":"+Base64.getEncoder
						.encodeToString(as.unsafeArray.asInstanceOf[Array[Byte]])+":"
					case b: Boolean => if b then "?1" else "?0"

		//
		// complex types
		//

		given paramSer(using Serialise[Item]): Serialise[Param] with
			extension (o: Param)
				def canon: String = ";"+o._1.canon + {o._2 match {
					case b: Boolean => ""
					case other => "="+other.canon
				}}

		given paramsSer(using Serialise[Param]): Serialise[Params] with
			extension (o: Params)
				def canon: String = o.map(_.canon).mkString

		given paramItemSer(using
			Serialise[Item], Serialise[Params]
		): Serialise[PItem] with
			extension (o: PItem)
				def canon: String = o.item.canon + o.params.canon

		given sfListSer(using
			Serialise[Item], Serialise[Params]
		): Serialise[IList] with
			extension (o: IList)
				def canon: String =
					o.items.map(i=>i.canon).mkString("("," ",")")+o.params.canon

		given sfDictSer(using
			Serialise[Item], Serialise[Param], Serialise[Params],
			Serialise[PItem], Serialise[IList]
		): Serialise[SfDict] with
			extension (o: SfDict)
				def canon: String = o.map{
					case (tk, PItem(true,params)) => tk.canon+params.canon
					case (tk, pit: PItem) => tk.canon+"="+pit.canon
					case (tk, lst: IList) => tk.canon+"="+lst.canon
				}.mkString(", ")
	end Serialise

}