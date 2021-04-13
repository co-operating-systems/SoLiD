package run.cosy.http.headers

import run.cosy.http.headers.Rfc8941.Serialise.Serialise

import java.util.Base64
import scala.collection.immutable.{ArraySeq, ListMap}


object Rfc8941 {

	object Parser {
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
		val sfInteger: P[IntStr] = (minus.?.with1 ~ R5234.digit.rep(1, 15)).string.map(IntStr.apply)
		val decFraction: P[String] = R5234.digit.rep(1, 3).string
		val signedDecIntegral: P[String] = (minus.?.with1 ~ R5234.digit.rep(1, 12)).map { case (min, i) =>
			min.map(_ => "-").getOrElse("") + i.toList.mkString
		}
		val sfDecimal: P[DecStr] =
			(signedDecIntegral ~ (`.` *> decFraction)).map { case (dec, frac) =>
				DecStr(dec, frac.toList.mkString) //todo: keep the non-empty list?
			}

		// first we have to check for decimals, then for integers, or there is the risk that the `.` won't be noticed
		// todo: optimisation would remove backtracking here
		val sfNumber: P[Number] = (sfDecimal.backtrack.orElse(sfInteger))

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
		val sfString: P[String] = (R5234.dquote *> (unescaped | escaped).rep0 <* R5234.dquote).map(_.mkString)
		val sfToken: P[Token] = ((R5234.alpha | P.charIn('*')) ~ (Rfc7230.tchar | P.charIn(':', '/')).rep0)
			.map { (c, lc) => Token((c :: lc).mkString) }
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
		val listMember: P[Paramed] = (sfItem | innerList)

		val sfList: P[SfList] =
			(listMember ~ ((ows *> P.char(',') *> ows).void.with1 *> listMember).rep0).map((p, lp) => p :: lp)

		val memberValue: P[Paramed] = (sfItem | innerList)
		//note: we have to go with parsing `=` first as parameters always returns an answer.
		val dictMember: P[DictMember] = (key ~ (P.char('=') *> memberValue).eitherOr(parameters))
			.map {
				case (k, Left(parameters)) => DictMember(k, parameters)
				case (k, Right(parameterized)) => DictMember(k, parameterized)
			}
		val sfDictionary: P[SfDict] =
			(dictMember ~ ((ows *> P.char(',') *> ows).with1 *> dictMember).rep0).map((dm, list) =>
				val x: List[DictMember] = dm :: list
					//todo: avoid this tupling
					ListMap.from(x.map((d: DictMember) => Tuple.fromProductTyped(d)))
		)
	}

	/**
	 *
 	 * Serialisation implementations for the RFC8941 types as defined in
	 * [[https://www.rfc-editor.org/rfc/rfc8941.html#section-4.1 §4.1 Serializing Structured Fields]]
	 * written as a type class so thqt it can easily be extended to give the result with non RFC8941 headers
	 * and work with different frameworks.
	 *
	 * todo: it may be that scalaz's Show class that uses a Cord
	 **/
	object Serialise {

		trait Serialise[-T]:
			extension (o: T)
			   //may be better if encoded directly to a byte string
				def canon: String

		given itemSer: Serialise[Item] with
			extension (o: Item)
				def canon: String = o match
					case i: IntStr => i.integer
					//todo: https://www.rfc-editor.org/rfc/rfc8941.html#ser-decimal
					case d: DecStr => d.integer + "." + d.dec
					case s: String =>  s""""$s""""
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

	}
	//
	//types uses by parser above
	//
	/** trait for Parameterizes types */
	sealed abstract class Paramed //would need to use http4s to get Renderable

	/**
	 * see [[https://www.rfc-editor.org/rfc/rfc8941.html#section-3.3 §3.3 Items]] of RFC8941.
	 * Note: all the Java implementations can take values that can be larger or a lot larger.
	 * So one should narrow the classes or be careful on serialisation, i.e. header construction.
	 * Note: Unit was added. It Allows us to have empty Item parameters. todo: check it's ok.
	 */
	type Item = Number | String | Token | ArraySeq[Byte] | Boolean
	type Param = (Token, Item)
	type Params = ListMap[Token, Item]
	type SfList = List[Paramed]
	type SfDict = ListMap[Token, Paramed|Params]

	def Param(tk: String, i: Item): Param = (Token(tk),i)
	def Params(ps: Param*): Params = ListMap(ps*)
	def SfDict(entries: (Token,Paramed|Params)*) = ListMap(entries*)
	/**
	 * dict-member    = member-key ( parameters / ( "=" member-value ))
	 * member-value   = sf-item / inner-list
	 *
	 * @param key    member-key
	 * @param values if InnerList with an empty list, then we have "parameters", else we have an inner list
	 */
	final
	case class DictMember(key: Token, values: Paramed|Params)

	/** Parameterized Item */
	final
	case class PItem(item: Item, params: Params) extends Paramed

	object PItem {
		def apply(item: Item): PItem = new PItem(item,ListMap())
		def apply(item: Item)(params: Param*): PItem = new PItem(item,ListMap(params*))
	}

	/** Inner List */
	final
	case class IList(items: List[PItem], params: Params) extends Paramed

	object IList {
		def apply(items: PItem*)(params: Param*): IList = new IList(items.toList,ListMap(params*))
	}

	sealed trait Number

	// todo: could one use List[Digit] instead of String, to avoid loosing type info?
	// todo: arguably Long would do just fine here too.
	final case class IntStr(integer: String) extends Number

	//Implementations may want to parse these decimals differently. We avoid loosing information
	//by not interpreting at this point. There may be a way not to loose that, but it would require
	//quite a lot of digging into the BigDecimal class' functioning. That would also tie this layer
	//to that choice, unless such choices could be passed `using` ops.
	//todo: details of how to map to BigDecimal are
	// here https://www.rfc-editor.org/rfc/rfc8941.html#ser-decimal
	final case class DecStr(integer: String, dec: String) extends Number

	final case class Token(t: String)

	implicit val token2PI: Conversion[Item,PItem] = (i: Item) => PItem(i)
	private def paramConversion(paras: Param*): Params = ListMap(paras*)
	implicit val paramConv: Conversion[Seq[Param],Params] = paramConversion
	
}