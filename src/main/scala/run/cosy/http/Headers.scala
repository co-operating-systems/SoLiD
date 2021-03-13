package run.cosy.http

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.{CustomHeader, ModeledCustomHeader, ModeledCustomHeaderCompanion, RawHeader}
import org.apache.commons.codec.net.URLCodec

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.Charset
import java.util.Locale
import scala.util.{Failure, Success, Try}
import scala.io.Codec
import scala.language.{existentials, implicitConversions}

object Encoding {
	val utf8 = Charset.forName("UTF-8")
	opaque type UnicodeString = String
	opaque type UrlEncoded = String
	
	implicit def toUnicode(str: String): UnicodeString = str
	
	extension (string: String)
		def asClean : UnicodeString = string
		def asEncoded : UrlEncoded = string
	
	extension (clean: UnicodeString)
	   def toString: String = clean
	   def urlEncode: UrlEncoded = URLEncoder.encode(clean, utf8)
	
	extension (encoded: UrlEncoded)
		def decode: Try[UnicodeString] = Try(URLDecoder.decode(encoded, utf8))
		def onTheWire: String = encoded
}
	
object Headers {
	import Encoding._
	
	/**
	 * The Slug header was first defined in [[https://tools.ietf.org/html/rfc5023#section-9.7 RFC 5023: Atom Publishing Protocol]]
	 *
	 * @param text: We manipulate Slug on sending and on receiving with Unicode Strings
	 */
	final case class Slug(text: UnicodeString) extends BetterCustomHeader[Slug]:
		override def renderInRequests = true
		override def renderInResponses = false
		override val companion = Slug
		override def value: String = text.urlEncode.toString

	
	object Slug extends BetterCustomHeaderCompanion[Slug] :
		override val name = "Slug"

		//override try to generalise later
		def parse(value: UrlEncoded): Try[UnicodeString] = value.decode
		
		//can an unapply return a Try in scala3
		//override - try to generalise later
		def unapply(h: HttpHeader): Option[UnicodeString] = h match {
			case _: (RawHeader | CustomHeader) => 
				if (h.lowercaseName == lowercaseName) parse(h.value.asEncoded).toOption else None
			case _ => None
		}
}


/**
 * To be extended by companion object of a custom header extending [[ModeledCustomHeader]].
 * Implements necessary apply and unapply methods to make the such defined header feel "native".
 */
abstract class BetterCustomHeaderCompanion[H <: BetterCustomHeader[H]] {
	def name: String
	def lowercaseName: String = name.toLowerCase(Locale.ROOT)
	
	final implicit val implicitlyLocatableCompanion: BetterCustomHeaderCompanion[H] = this
}

/**
 * Support class for building user-defined custom headers defined by implementing `name` and `value`.
 * By implementing a [[BetterCustomHeader]] instead of [[CustomHeader]] directly, all needed unapply
 * methods are provided for this class, such that it can be pattern matched on from [[RawHeader]] and
 * the other way around as well.
 */
abstract class BetterCustomHeader[H <: BetterCustomHeader[H]] extends CustomHeader { this: H =>
	def companion: BetterCustomHeaderCompanion[H]

	final override def name = companion.name
	final override def lowercaseName = name.toLowerCase(Locale.ROOT)
}
