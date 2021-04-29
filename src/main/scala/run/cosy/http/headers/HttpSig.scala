package run.cosy.http.headers

import scala.util.Try
import _root_.akka.http.scaladsl.model.headers.{HttpCredentials,GenericHttpCredentials}

/**
 * Parameters that come with an `Authorization: HttpSig proof=sig1` header
 *
 * @param proofName: The name of the signature to look for
 * todo later: see if one can built an extractor for this for Akka
 * */
case class HttpSig(proofName: Rfc8941.Token)

object HSCredentials {
	def unapply(cred: HttpCredentials): Option[HttpSig] = {
		cred match
			case GenericHttpCredentials("HttpSig",_,parameters) =>
				parameters.get("proof").flatMap(str => Try(HttpSig(Rfc8941.Token(str))).toOption)
			case _ => None
	}
}
