package run.cosy.http.headers

import _root_.akka.http.scaladsl.model.{HttpHeader, ParsingException, Uri}
import _root_.akka.http.scaladsl.model.headers.{CustomHeader, RawHeader}


import cats.parse.Parser       
import com.nimbusds.jose.util.Base64
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion, HTTPHeaderParseException, UnableToCreateSigHeaderException}

import java.security.{PrivateKey, PublicKey, Signature}
import java.time.Instant
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}
import run.cosy.http.headers.Rfc8941
import Rfc8941.{IList, Item, PItem, Parameterized, Params, SfDict, SfInt, SfList, SfString, Token}
import run.cosy.http.Encoding.UnicodeString

import scala.collection.immutable


/**
 * [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-4.1 4.1 The 'Signature-Input' HTTP header]]
 * defined in "Signing HTTP Messages" HttpBis RFC.
 * Since version 03 signature algorithms have been re-introduced, but we only implement "hs2019" for simplicity.
 * @param text
 */
final case class `Signature-Input`(sig: SigInputs) extends BetterCustomHeader[`Signature-Input`]:
	override def renderInRequests = true
	override def renderInResponses = true
	override val companion = `Signature-Input`
	override def value: String =
		import Rfc8941.Serialise.given
		sig.si.map{(tk,si)=> (tk,si.il)}.asInstanceOf[Rfc8941.SfDict].canon



object `Signature-Input` extends BetterCustomHeaderCompanion[`Signature-Input`]:
	override val name = "Signature-Input"

	def apply[M](name: Rfc8941.Token, sigInput: SigInput)(using SelectorOps[M]): `Signature-Input` =
		`Signature-Input`(SigInputs(name,sigInput))

	def parse[M](value: String)(using SelectorOps[M]): Try[SigInputs] =
		Rfc8941.Parser.sfDictionary.parseAll(value) match
			case Left(e) => Failure(HTTPHeaderParseException(e,value))
			case Right(lm) => Success(SigInputs.filterValid(lm))

	def unapply[M](h: HttpHeader)(using SelectorOps[M]): Option[SigInputs] =
		h match
		case _: (RawHeader | CustomHeader) if h.lowercaseName == lowercaseName => parse(h.value).toOption
		case _ => None

end `Signature-Input`

/**
 * SigInputs are Maps from Signature Names to SigInput entries that this
 * server understands.
 *
 * @param value
 * @return
 */
final case class SigInputs private(val si: ListMap[Rfc8941.Token,SigInput]) extends AnyVal {
	def get(key: Rfc8941.Token): Option[SigInput] = si.get(key)
	def append(more: SigInputs): SigInputs = new SigInputs(si ++ more.si)
	def append(key: Rfc8941.Token, sigInput: SigInput): SigInputs = new SigInputs(si + (key -> sigInput))
}

object SigInputs:
	/* create a SigInput with a single element */
	def apply[M](name: Rfc8941.Token, siginput: SigInput)(using SelectorOps[M]) =
		new SigInputs(ListMap(name->siginput))

	/**
	 * Filter out the inputs that this framework does not accept.
	 **/
	def filterValid[M](lm: SfDict)(using SelectorOps[M]): SigInputs = SigInputs(lm.collect{
		case (sigName, SigInput(sigInput)) => (sigName,sigInput)
	})


/**
 * A SigInput is a valid Signature-Input build on an Rfc8941 Internal List.
 * restricted to those this server can understand.
 * todo: An improved version would be more lenient, allowing opt-in refinements.
 *
 * As a Validated data structure, we can keep all the data present in a header for a particular
 * signature, as that is needed to verify the signature itself. Indeed extra attributes will be
 * vital to verify a signatue, since the data from this header is part of the signature
 * @param il
 */
final case class SigInput private(val il: IList) extends AnyVal {
	import Rfc8941.Serialise.given
	import SigInput.{createdTk,expiresTk,keyidTk,algTk, nonceTk}
	def headers: Seq[String] = il.items.map{ case PItem(SfString(str),_) => str}
	def headerItems: Seq[PItem[SfString]] = il.items.map(_.asInstanceOf[PItem[SfString]])

	def keyid: Rfc8941.SfString = il.params.get(keyidTk).get.asInstanceOf[Rfc8941.SfString]

	def alg: Option[String] = il.params.get(algTk).collect{case SfString(str) => str}
	def created: Option[Long] = il.params.get(createdTk).collect{case SfInt(time) => time}
	def expires: Option[Long] = il.params.get(expiresTk).collect{case SfInt(time) => time}
	def nonce: Option[String] = il.params.get(nonceTk).collect{case SfString(str) => str}

	def isValidAt(i: Instant, shift: Long=0): Boolean =
		created.map(_ - shift <= i.getEpochSecond).getOrElse(true) &&
			expires.map(_ + shift >= i.getEpochSecond).getOrElse(true)

	def canon: String = il.canon

}

object SigInput {

	/** registered metadata parameters as per [[https://www.ietf.org/archive/id/draft-ietf-httpbis-message-signatures-04.html#section-5.2.2 §5.2.2]].
	 * To avoid them being confused with pattern matches variables enclose in ` `.*/
	val algTk = Token("alg")
	val createdTk = Token("created")
	val expiresTk = Token("expires")
	val keyidTk = Token("keyid")
	val nonceTk = Token("nonce")

	val registeredParams = Seq(algTk,createdTk,expiresTk,keyidTk,nonceTk)

	val Empty = ListMap.empty[Token, Item]

	def apply[M](innerListStr: String)(using SelectorOps[M]): Option[SigInput] =
		Rfc8941.Parser.innerList.parseAll(innerListStr).toOption.flatMap(il => SigInput[M](il))

	def apply[M](il: IList)(using SelectorOps[M]): Option[SigInput] =
		if valid(il) then Some(new SigInput(il)) else None

	//this is really functioning as a constructor in pattern matching contexts
	def unapply[M](pzd: Parameterized)(using SelectorOps[M]): Option[SigInput] =
		pzd match
			case il: IList if valid(il) => Some(new SigInput(il))
			case _                      => None


	/**
	 * A Valid SigInput IList has an Interal list of parametered SfStrings which must contain
	 * a keyid attribute.
	 * */
	def valid[H](il: IList)(using o: SelectorOps[H]): Boolean =
		def headersOk = il.items.forall {
				case p: PItem[SfString] => o.valid(p)
				case _ => false
			}
		def paramsOk = il.params.forall {
			case (`keyidTk`, item: SfString) => true
			case (`createdTk`, _: SfInt) | (`expiresTk`, _: SfInt)   => true
			case (`algTk`, _: SfString)  | (`nonceTk`, _: SfString)  => true
			// we are lenient on non-registered params
			case (attr, _) => !registeredParams.contains(attr)
		}
		def keyIdExists = il.params.get(`keyidTk`).isDefined
		keyIdExists && headersOk && paramsOk
	end valid
	
}
