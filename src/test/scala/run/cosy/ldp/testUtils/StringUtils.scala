package run.cosy.ldp.testUtils


import akka.http.scaladsl.model.{ContentType, ContentTypes, HttpEntity, HttpMessage, HttpRequest, HttpResponse}

import java.util.Base64
import scala.IArray
import scala.annotation.tailrec

object StringUtils:
	private val singleSlsh = raw"\\\n *".r

	extension (str: String)
		/**
		 * following [[https://tools.ietf.org/html/rfc8792#section-7.2.2 RFC8792 §7.2.2]] single
		 * slash line unfolding algorithm
		 */
		def rfc8792single: String = singleSlsh.replaceAllIn(str.stripMargin,"")

		def base64Decode: IArray[Byte] = IArray.unsafeFromArray(Base64.getDecoder.decode(str))

		def toRfc8792single(leftPad: Int=4, maxLineLength: Int=79): String =
			@tailrec
			def lengthSplit(remaining: String, buf: List[String]=List(), firstLine: Boolean=true): List[String] =
				if remaining.isEmpty then buf.reverse
				else
					val n = if firstLine then maxLineLength else maxLineLength - leftPad
					val (headStr, remainingStr) = remaining.splitAt(n)
					lengthSplit(remainingStr,headStr::buf,false)
			end lengthSplit
			str.split("\\R").toList.map{line =>  lengthSplit(line).mkString("\\\n"+(" "*leftPad))}.mkString("\n")

	extension (msg: HttpMessage)
		def documented: String =
			val l: List[String] = msg match
				case req: HttpRequest =>
					import req._
					s"${method.value} $uri ${protocol.value}" :: {
						for {h <- headers.toList} yield s"${h.name}: ${h.value}"
					}
				case res: HttpResponse =>
					import res._
					protocol.value +" "+ status.value :: {
						for {h <- headers.toList } yield s"${h.name}: ${h.value}"
					}
			import msg._
			val ct = entity match
				case HttpEntity.Empty => List()
				case e: HttpEntity if e.contentType != ContentTypes.NoContentType =>
					("Content-Type: "+e.contentType) ::
					e.contentLengthOption.map("Content-Length: "+_).toList
				case _ => List()
			(l ::: ct).mkString("\n")



