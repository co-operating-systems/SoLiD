package run.cosy.http.auth

import akka.http.scaladsl.model.Uri
import run.cosy.http.headers.Rfc8941

import scala.util.Try


trait Agent
trait Keyid:
	def keyId: String

case class KeyidAgent(keyId: String) extends Agent with Keyid

case class WebKeyidAgent(uri: Uri) extends Agent with Keyid:
	def keyId = uri.toString()

object WebKeyidAgent:
	def apply(keyId: Rfc8941.SfString): Option[WebKeyidAgent] =
		Try(WebKeyidAgent(Uri(keyId.asciiStr))).toOption

class Anonymous extends Agent
object WebServerAgent extends Agent

