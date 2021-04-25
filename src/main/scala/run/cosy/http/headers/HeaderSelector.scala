package run.cosy.http.headers

import scala.util.{Failure, Try}
import run.cosy.http.{InvalidSigException, headers}
import run.cosy.http.headers.Rfc8941
import headers.SigInput

/**
 * Header info specifying properties of a header for Signing Http Messages,
 * and how to create them
 *
 * @tparam HttpMsg the type of the Http Message (req or response) to look at when
 *                 constructing the signing strings for that header
 **/
trait HeaderSelector[HttpMessage] {
	def lowercaseName: String
	lazy val special: Boolean = lowercaseName.startsWith("@")
	def valid(params: Rfc8941.Params): Boolean = params.isEmpty
	/**
	 *  @return a signing string for this header for a given HttpMessage
	 *          Fail if the header does not exist or is malformed
	 **/
	def signingString(msg: HttpMessage): Try[String]
}

trait DictSelector[HM] extends HeaderSelector[HM] {
	val validParams = Set(SelectorOps.key)
	/**
	 *
	 * @param key the element of the dictionary to build a signing string on
	 * @return a signing string for the selected entry of the sf-dictionary encoded header.
	 *          Fail if the header does not exist or is malformed
	 **/
	def signingString(msg: HM, key: Rfc8941.Token): Try[String]

	override def valid(params: Rfc8941.Params): Boolean =
		params.keys.isEmpty || params.keySet == validParams

}

trait ListSelector[HM] extends HeaderSelector[HM] {
	val validParams = Set(SelectorOps.prefix)
	/**
	 *  @param prefix the first n elements of the list to select
	 *  @return a signing string for this first `prefix` elements of the sf-list encoded header
	 *          Fail if the header does not exist or is malformed
	 **/
	def signingString(msg: HM, prefix: Rfc8941.SfInt): Try[String]

	override def valid(params: Rfc8941.Params): Boolean =
		params.keys.isEmpty || params.keySet == validParams

}

object `@signature-params` {
	val lowercaseName = "@signature-params"
	val pitem = Rfc8941.PItem(Rfc8941.SfString(lowercaseName))
	def signingString(sigInput: SigInput): String = s""""@signature-params": ${sigInput.canon}"""
}

/**
 *
 * @param selectorFor
 * @tparam HM
 */
case class SelectorOps[HM](selectorFor: Map[Rfc8941.SfString,HeaderSelector[HM]] ):

	import Rfc8941.Serialise.given
	import SelectorOps.{prefix,key}

	/** add new selectors to this one */
	def append(selectors: HeaderSelector[HM]*): SelectorOps[HM] =
		val pairs = for (selector <- selectors)
			yield (Rfc8941.SfString(selector.lowercaseName) -> selector)
		SelectorOps(selectorFor ++ pairs.toSeq)

	def valid(selector: Rfc8941.PItem[Rfc8941.SfString]): Boolean =
		selectorFor.get(selector.item).map(_.valid(selector.params)).getOrElse(false)

	def select(msg: HM, hselector: Rfc8941.PItem[Rfc8941.SfString]): Try[String] = 
		def withParams(hs: HeaderSelector[HM], params: Rfc8941.Params): Try[String] =
			hs match {
				case ds: DictSelector[HM] =>
					params.get(prefix) match
						case Some(tk: Rfc8941.Token) if params.size == 1 => ds.signingString(msg, tk)
						case None if params.size == 0 => ds.signingString(msg)
						case _ => Failure(InvalidSigException(s"""Dictionary Selector »${hselector.canon}« is malformed """))
				case ls: ListSelector[HM] =>
					params.get(key) match
						case Some(pos: Rfc8941.SfInt) if hselector.params.size == 1 => ls.signingString(msg, pos)
						case None if hselector.params.size == 0 => ls.signingString(msg)
						case _ => Failure(InvalidSigException(s"""List Selector »${hselector.canon}« is malformed"""))
				case normal =>
					if params.size == 0 then normal.signingString(msg)
					else Failure(InvalidSigException(s"""Normal Selector »${hselector.canon}« is malformed"""))
			}
		end withParams

		for {
			hs <- selectorFor.get(hselector.item)
				.toRight(InvalidSigException(s"Header ${hselector.item} is not supported")).toTry
			str <- withParams(hs, hselector.params)
		} yield str
	end select

object SelectorOps:
	val prefix = Rfc8941.Token("prefix")
	val key = Rfc8941.Token("key")

	def apply[Msg](headers: HeaderSelector[Msg]*): SelectorOps[Msg] =
		val pairs = for (header <- headers) yield (Rfc8941.SfString(header.lowercaseName) -> header)
		new SelectorOps(Map(pairs*))

