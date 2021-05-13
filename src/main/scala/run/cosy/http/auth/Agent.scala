package run.cosy.http.auth

import akka.http.scaladsl.model.Uri
import run.cosy.http.headers.Rfc8941

import java.security.PublicKey
import scala.util.Try


trait Agent
trait Keyidentifier:
	def keyId: String
trait KeyId extends Keyidentifier:
	def keyIdUri: Uri
	def keyId = keyIdUri.toString()
trait PubKey:
	def pubKey: PublicKey

case class KeyidSubj(keyId: String, pubKey: PublicKey) extends Agent with Keyidentifier with PubKey

/**
 * KeyId agents interpret the `keyid`  field of Message Signing as a URI.
 * @param keyIdUri
 * @param pubKey
 */
case class KeyIdAgent(keyIdUri: Uri, pubKey: PublicKey) extends Agent with KeyId with PubKey

object KeyIdAgent:
	@deprecated("used only to get framework going")
	case class TmpKeyIdAgent(keyIdUri: Uri) extends KeyId with Agent

	@deprecated("this is only used for temporarily to get some things going")
	def apply(keyId: String): Option[TmpKeyIdAgent] =
		Try(TmpKeyIdAgent(keyId)).toOption

class Anonymous extends Agent
object WebServerAgent extends Agent

