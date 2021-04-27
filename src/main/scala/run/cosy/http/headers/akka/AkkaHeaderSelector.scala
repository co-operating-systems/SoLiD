package run.cosy.http.headers.akka

import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpHeader, HttpMessage, HttpRequest, HttpResponse,Uri}
import run.cosy.http.UnableToCreateSigHeaderException
import run.cosy.http.headers.{DictSelector, HeaderSelector, ListSelector, Rfc8941, SelectorOps}
import Rfc8941.{PItem, Params, Serialise, SfDict, SfString}
import Rfc8941.Serialise.given

import java.util.Locale
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.ListMap

trait AkkaHeaderSelector extends HeaderSelector[HttpMessage]:
	/** @return the collated cleaned up values of the headers with the same name */
	protected final def collate(values: Seq[HttpHeader]): Try[String] =
		values match
		case Nil => Failure(UnableToCreateSigHeaderException(s"""No header '$lowercaseName' in request"""  ))
		case nonNil => Success(
			nonNil.map( //remove obsolete line folding and trim
				_.value.split('\n').map(_.trim).mkString(" ")
			).mkString(", ")
		)

	/** the lowercase name followed by colon and 1 space */
	final def headerName: String = s""""$lowercaseName": """
	/**
	 * default for non-rfc 8941 headers.
	 **/
	override def signingString(msg: HttpMessage): Try[String] =
		collate(filterHeaders(msg)).map(headerName+_)

	/** override this method. */
	protected def filterHeaders(msg: HttpMessage): Seq[HttpHeader]

/**
 * Akka's builtin header parsers have specialised parsers for the
 * most well known headers.
 **/
trait TypedAkkaSelector[HdrType<:HttpHeader:scala.reflect.ClassTag] extends AkkaHeaderSelector:
	def akkaCompanion: ModeledCompanion[HdrType]
	override val lowercaseName: String = akkaCompanion.lowercaseName
	protected def filterHeaders(msg: HttpMessage): Seq[HttpHeader] = msg.headers[HdrType]

/** for all headers for which Akka HTTP does not provide a built-in parser */
trait UntypedAkkaSelector extends AkkaHeaderSelector:
	protected def filterHeaders(msg: HttpMessage): Seq[HttpHeader] =
		msg.headers.filter(_.lowercaseName() == lowercaseName)

/** todo: the UntypedAkkaSelector inheritance may not be long term */
trait AkkaDictSelector extends DictSelector[HttpMessage] with UntypedAkkaSelector:
	override def signingString(msg: HttpMessage): Try[String] = sfDictParse(msg).map(headerName + _.canon)
	override def signingString(msg: HttpMessage, key: Rfc8941.Token): Try[String] =
		for {
			dict <- sfDictParse(msg)
			value <- dict.get(key).toRight(UnableToCreateSigHeaderException(
						s"could not find $key in header [$dict]")
					).toTry
		} yield headerName + value.canon

	def sfDictParse(msg: HttpMessage): Try[SfDict] =
		for {
			headers <- filterHeaders(msg) match {
				case Seq() => Failure(UnableToCreateSigHeaderException(s"No headers »$lowercaseName« in request"))
				case nonempty => Success(nonempty)
			}
			str <- collate(headers)
			sfDict <- parse(str)
		} yield sfDict

	def parse(hdrStr: String): Try[SfDict] =
		Rfc8941.Parser.sfDictionary.parseAll(hdrStr) match
			case Left(err) => Failure(
				UnableToCreateSigHeaderException(
					s"parsing problem on header [$hdrStr] caused by $err"))
			case Right(sfDict) => Success(sfDict)

/**
 * Wait to implement:
 * It feels like [[https://www.rfc-editor.org/rfc/rfc8941#name-lists RFC8941 §3.1 Lists]] may be
 * a better fit for what is intended here. Inner Lists on headers require there to be only 1 header of a given
 * name as multiple lists cannot be split. Or perhaps the same process would work there too?
 * Let us wait to see how the spec evolves.
 */
trait AkkaListSelector extends ListSelector[HttpHeader] with UntypedAkkaSelector:
	override def signingString(msg: HttpHeader, prefix: Rfc8941.SfInt): Try[String] = ???
//	override def signingString(msg: HttpHeader): Try[String] = ???

object authorization extends TypedAkkaSelector[Authorization]:
	def akkaCompanion = Authorization

object `cache-control` extends TypedAkkaSelector[`Cache-Control`]:
	def akkaCompanion = `Cache-Control`

object date extends TypedAkkaSelector[Date]:
	def akkaCompanion = Date

object etag extends TypedAkkaSelector[ETag]:
	def akkaCompanion = ETag

object host extends TypedAkkaSelector[Host]:
	def akkaCompanion = Host

object signature extends AkkaDictSelector:
	override val lowercaseName: String = "signature"

/** Content-Type is special in Akka, as it is associated with the entity body,
 * and is not modelled as a header */
object `content-type` extends UntypedAkkaSelector:
	override val lowercaseName: String = "content-type"
	override def signingString(msg: HttpMessage): Try[String] =
		import akka.http.scaladsl.model.ContentTypes.NoContentType
		msg.entity.contentType match
		case NoContentType => Failure(
			UnableToCreateSigHeaderException(s"""No header '$lowercaseName' in request"""))
		case ct => Success(s""""$lowercaseName": $ct""")

/**
 * Content-Length is special, and is associated with the entity body
 */
object `content-length` extends UntypedAkkaSelector:
	override val lowercaseName: String = "content-length"
	override def signingString(msg: HttpMessage): Try[String] =
		msg.entity.contentLengthOption match
			case None => Failure(
				UnableToCreateSigHeaderException(s"""No header '$lowercaseName' in request"""))
			case Some(cl) => Success(s""""$lowercaseName": $cl""")


object `digest` extends UntypedAkkaSelector:
	override val lowercaseName: String = "digest"

/**
 * `@request-target` is very special. It does not work on individual headers, but
 *    on the whole request
 */
object `@request-target` extends AkkaHeaderSelector:
	override val lowercaseName: String = "@request-target"

	/** this is perhaps the only object where this mathod is not used */
	override protected def filterHeaders(msg: HttpMessage): Seq[HttpHeader] = ???

	override def signingString(msg: HttpMessage): Try[String] =
		msg match
		case req: HttpRequest => Success(s""""$lowercaseName$": """ +
			req.method.value.toLowerCase(Locale.ROOT) + " " + {
				if req.uri.path.isEmpty then Uri.Path./ else req.uri.path
			} + {
			req.uri.rawQueryString.map("?" + _).getOrElse("")
		})
		case _: HttpResponse => Failure(
			UnableToCreateSigHeaderException("cannot build @request-target for response message"))

given akkaSelectorOps: SelectorOps[HttpMessage] =
	SelectorOps(authorization,`cache-control`,`content-type`,`content-length`,
		date,`digest`,etag,host,`@request-target`)
