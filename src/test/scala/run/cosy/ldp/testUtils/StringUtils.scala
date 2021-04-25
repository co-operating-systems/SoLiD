package run.cosy.ldp.testUtils


import java.util.Base64
import scala.IArray

object StringUtils:
	private val singleSlsh = raw"\\\n *".r

	extension (str: String)
		/**
		 * following [[https://tools.ietf.org/html/rfc8792#section-7.2.2 RFC8792 §7.2.2]] single
		 * slash line unfolding algorithm
		 */
		def rfc8792single: String = singleSlsh.replaceAllIn(str.stripMargin,"")

		def base64Decode: IArray[Byte] = IArray.unsafeFromArray(Base64.getDecoder.decode(str))

