package run.cosy.http.headers

import akka.http.scaladsl.model.{HttpHeader, ParsingException}
import akka.http.scaladsl.model.headers.{CustomHeader, RawHeader}
import run.cosy.http.headers.Rfc8941.{Bytes, IList, PItem, SfDict}
import run.cosy.http.{BetterCustomHeader, BetterCustomHeaderCompanion, HTTPHeaderParseException, headers}

import scala.collection.immutable
import scala.collection.immutable.{ArraySeq, ListMap}
import scala.util.{Failure, Success, Try}

/**
 *  [[https://tools.ietf.org/html/draft-ietf-httpbis-message-signatures-03#section-4.2 §4.2 The Signature HTTP header]] defined in "Signing HTTP Messages" HttpBis RFC.
 * @param text
 */
final case class Signature(sig: Signatures) extends BetterCustomHeader[Signature]:
	override def renderInRequests = true
	override def renderInResponses = true
	override val companion = Signature
	override def value: String =
		import Rfc8941.Serialise.given
		sig.sigmap.asInstanceOf[Rfc8941.SfDict].canon


object Signature extends BetterCustomHeaderCompanion[Signature]:
	override val name = "Signature"

	def parse(value: String): Try[Signatures] =
		Rfc8941.Parser.sfDictionary.parseAll(value) match
			case Left(e) => Failure(HTTPHeaderParseException(e,value))
			case Right(lm) => Success(Signatures(lm))

	def unapply(h: HttpHeader): Option[Signatures] =
		h match
			case _: (RawHeader | CustomHeader) if h.lowercaseName == lowercaseName => parse(h.value).toOption
			case _ => None
end Signature

/**
 * A Signature is an SfDict, refined to contain only PItems of Arrays of Bytees.
 * We want to keep potential attributes as they could be useful. */
final class Signatures private(val sigmap: ListMap[Rfc8941.Token, PItem[Bytes]]) extends AnyVal:
	@throws[ParsingException]
	def get(signame: String): Option[Bytes] = sigmap.get(Rfc8941.Token(signame)).map(_.item)

	//add the signature to the list.
	def add(signame: Rfc8941.Token, signbytes: Bytes) =
		new Signatures(sigmap.updated(signame,PItem(signbytes)))

object Signatures:
	def apply(lm: SfDict): Signatures = new Signatures(filterValid(lm))

	def filterValid(lm: SfDict): ListMap[Rfc8941.Token, PItem[Bytes]] = lm.collect {
		case (sigName, pi @ PItem(bytes: ArraySeq[_],attr)) =>
			// if it is an ArraySeq, it is a ByteArraySequence!
			(sigName, pi.asInstanceOf[PItem[Bytes]] )
	}

	def apply(signame: Rfc8941.Token, signbytes: Bytes): Signatures =
		new Signatures(ListMap(signame->PItem(signbytes)))
end Signatures




