package run.cosy.http

import akka.http.scaladsl.model.{ContentType, HttpHeader, StatusCode, Uri}
import org.w3.banana.jena.Jena.Rdf

import scala.util.control.NoStackTrace

//todo: these were used for a different project. It is not clear if they are really needed here.

trait WebException extends java.lang.RuntimeException with NoStackTrace with Product with Serializable
case class HTTPException(response: ResponseSummary, msg: String) extends WebException
case class StatusCodeException(response: ResponseSummary) extends WebException
case class ConnectionException(resourceUri: String, e: Throwable) extends WebException
case class NodeTranslationException(graphLoc: String, problemNode: Rdf#Node, e: Throwable) extends WebException
case class MissingParserException(initialContent: String) extends WebException
case class ParseException(response: ResponseSummary, initialContent: String, e: Throwable) extends WebException

case class ResponseSummary(on: Uri, code: StatusCode, header: Seq[HttpHeader], respTp: ContentType)

trait AuthExc extends WebException
case class CryptoException(msg: String) extends AuthExc
case class AuthException(response: ResponseSummary, msg: String) extends AuthExc
case class InvalidCreatedFieldException(msg: String) extends AuthExc
case class InvalidExpiresFieldException(msg: String) extends AuthExc