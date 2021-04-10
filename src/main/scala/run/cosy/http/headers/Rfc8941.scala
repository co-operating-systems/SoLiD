package run.cosy.http.headers

import cats.parse.Rfc5234.wsp
import run.cosy.http.headers.ParamItems.Key

import java.util.Base64
import scala.collection.immutable.ArraySeq
//import run.cosy.http.headers.Rfc7230.obsText

import scala.collection.immutable.ListMap


object Rfc8941 {
	import ParamItems._
	import cats.parse.{Parser => P, Parser0 => P0}
	import cats.parse.{Rfc5234}
	import Rfc5234.{char,digit,dquote,bit, alpha,sp}

	import run.cosy.http.headers.Rfc7230.ows
	import cats.parse.Numbers.{signedIntString,nonNegativeIntString}


	lazy val sfDictionary: P[ListMap[Key,Parameterized]] =
		(dictMember ~ ((ows *> P.char(',') *> ows).with1 *> dictMember).rep0).map((dm, list) =>
			lazy val x: List[DictMember] = dm::list
				//todo: avoid this tupling
			ListMap.from(x.map((d: DictMember)=> Tuple.fromProductTyped(d)))
		)

	//note: we have to go with parsing `=` first as parameters always returns an answer.
	lazy val dictMember: P[DictMember] = (Key ~ (P.char('=') *> memberValue).eitherOr(parameters))
		.map{
			case (k, Left(parameters))   => DictMember(k, PItem((),parameters))
			case (k, Right(parameterized)) => DictMember(k, parameterized)
		}

	lazy val memberValue: P[Parameterized] = (sfItem | innerList)

	lazy val sfList: P[SFList] =
		(listMember ~ ((ows *> P.char(',') *> ows).void.with1 *> listMember).rep0).map((p, lp)=> p::lp)

	lazy val listMember: P[Parameterized] = (sfItem | innerList)

	lazy val innerList: P[InnerList] =
		(((P.char('(') ~ sp.rep0) *> ((sfItem ~ (sp *> sfItem).rep0 <* sp.rep0).?) <* P.char(')')) ~ parameters)
			.map{
				case (Some(pi, lpi), params) => InnerList(pi::lpi, params)
				case (None,params) => InnerList(List(),params)
			}

	lazy val sfItem: P[PItem] = (bareItem ~ parameters).map(PItem.apply)

	//note: parameters always returns an answer (the empty list) as everything can have parameters
	//todo: this is not exeactly how it is specified, so check here if something goes wrong
	lazy val parameters: P0[ParamItems.Parameters] =
		(P.char(';') *> ows *> parameter).rep0.orElse(P.pure(List())).map{ list =>
			ListMap.from[Key,Item](list.iterator)
		}

	lazy val parameter: P[ParamItems.Parameter] =
		(Key ~ ( P.char('=') *> bareItem).orElse(P.unit))

	lazy val `*` : P[Char] = P.charIn('*')
	lazy val Key: P[Key] = ((lcalpha | `*`) ~ (lcalpha | digit | P.charIn('_','-','.','*')).rep0)
										.map((c,lc) => (c::lc).mkString)

	lazy val lcalpha: P[Char] = P.charIn(0x61.toChar to 0x7a.toChar) | P.charIn('a' to 'z')

	lazy val bareItem: P[ParamItems.Item] = P.oneOf(sfNumber::sfString::sfToken::sfBinary::sfBoolean::Nil)

	lazy val sfInteger: P[IntStr] = (P.char('-').?.with1 ~ digit.rep(1,15)).string.map(IntStr.apply)

	lazy val sfDecimal: P[DecStr] =
		(signedDecIntegral ~ (P.char('.') *> decFraction)).map { case (dec,frac) =>
		  DecStr(dec,frac.toList.mkString) //todo: keep the non-empty list?
	}
	lazy val signedDecIntegral: P[String] = (P.char('-').?.with1 ~ digit.rep(1,12)).map { case (min, i) =>
		min.map(_ => "-").getOrElse("")+i.toList.mkString
	}
	lazy val decFraction: P[String] = digit.rep(1,3).string

	// first we have to check for decimals, then for integers, or there is the risk that the `.` won't be noticed
	// todo: optimisation would remove backtracking here
	lazy val sfNumber : P[ParamItems.Number] = (sfDecimal.backtrack.orElse(sfInteger))

	/**
	 * unescaped      =  SP / %x21 / %x23-5B / %x5D-7E
	 * note: this is similar to [[run.cosy.http.headers.Rfc5234]] except
	 *       except that tabs are not allowed an anything above the ascii char set.
	**/
	private val bs = '\\'
	lazy val backslash: P[Unit] = P.char(bs)
	lazy val unescaped: P[Char] =
		P.charIn(' ', 0x21.toChar)
			.orElse(P.charIn(0x23.toChar to 0x5b.toChar))
			.orElse(P.charIn(0x5d.toChar to 0x7e.toChar))

	lazy val escaped: P[Char] = (backslash *> (P.charIn(bs,'"')))
	lazy val sfString: P[String] = (dquote *> (unescaped | escaped).rep0 <* dquote).map(_.mkString)
	lazy val sfToken: P[Token] = ((Rfc5234.alpha | P.charIn('*')) ~ (Rfc7230.tchar | P.charIn(':','/')).rep0 )
		.map{(c,lc) => Token((c::lc).mkString)}


	private val `:` : P[Unit] = P.char(':')
	lazy val base64: P[Char] = (alpha | digit | P.charIn('+',bs,'='))
	lazy val sfBinary: P[ArraySeq[Byte]] = (`:` *> base64.rep0 <* `:`).map { chars =>
		ArraySeq.unsafeWrapArray(Base64.getDecoder.decode(chars.mkString))
	}

	private val `?` : P[Unit] = P.char('?')
	lazy val sfBoolean =  (`?` *> boolean)
	lazy val boolean: P[Boolean] = Rfc5234.bit.map( _ == '1')
}

sealed abstract class Parameterized //would need to use http4s to get Renderable

object ParamItems {
	/**
	 * see [[https://www.rfc-editor.org/rfc/rfc8941.html#section-3.3 §3.3 Items]] of RFC8941.
	 * Note: all the Java implementations can take values that can be larger or a lot larger.
	 *    So one should narrow the classes or be careful on serialisation, i.e. header construction.
	 * Note: Unit was added. It Allows us to have empty Item parameters. todo: check it's ok.
	 */
	type Item = Number | String | Token | ArraySeq[Byte] | Boolean | Unit
	type Key = String
	type Parameter = (Key,Item)
	type Parameters = ListMap[Key,Item]
	type SFList = List[Parameterized]

	/**
	 * dict-member    = member-key ( parameters / ( "=" member-value ))
	 * member-value   = sf-item / inner-list
	 * @param key member-key
	 * @param values if InnerList with an empty list, then we have "parameters", else we have an inner list
	 */
	final
	case class DictMember(key: Key, values: Parameterized)

	final
	case class PItem(item: Item, params: Parameters=ListMap()) extends Parameterized

	final
	case class InnerList(items: List[PItem],  params: Parameters=ListMap()) extends Parameterized

	// todo: use List[Digit] instead of String, to avoid loosing type info
	trait Number
	final case class IntStr(integer: String) extends Number

	//todo: same as Above
	//Implementations may want to parse these decimals differently. We avoid loosing information
	//by not interpreting at this point. There may be a way not to loose that, but it would require
	//quite a lot of digging into the BigDecimal class' functioning.
	final case class DecStr(integer: String, dec: String) extends Number

	final case class Token(t: String)
}