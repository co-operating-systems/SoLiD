package run.cosy.ldp.fs

import akka.actor.typed.Behavior

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.util.Try


		
object Attributes {
	//todo: currently we use attributes to generate the type, but arguably, when we create an object, we don't
	//     need attributes, or even verifying where the link points to since we just created it. 
	//     So we could add methods to create a link, and fill in the attributes and linkedTo object
	
	//todo: I don't get the feeling that Java Paths are efficiently encoded. It looks like there is room for
	//   optimization. Perhaps using Akka paths would be better.
	
	import run.cosy.ldp.fs.Attributes.Attributes

	sealed trait Attributes(path: Path, att: BasicFileAttributes, collectedAt: Instant)

	def apply(fileName: Path, att: BasicFileAttributes, 
	          collectedAt: Instant = Instant.now()): Attributes = 
		if (att.isDirectory) DirAtt(fileName,att,collectedAt)
		else if (att.isSymbolicLink) {
			val linkTo = Files.readSymbolicLink(fileName)
			SymLink(fileName, linkTo, att, collectedAt)
		}
		else OtherAtt(fileName,att,collectedAt)

	def forPath(path: Path): Try[Attributes] =
		import java.nio.file.LinkOption.NOFOLLOW_LINKS
		import java.nio.file.attribute.BasicFileAttributes
		Try {
			val att = Files.readAttributes(path, classOf[BasicFileAttributes], NOFOLLOW_LINKS)
			Attributes(path,att)
		}
	
	//todo: may want to remove this later. Just here to help me refactor code
	sealed trait ActorFileAttr extends Attributes
	
	case class DirAtt private[Attributes] (
		path: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends ActorFileAttr with Attributes(path,att,collectedAt)

	case class SymLink private[Attributes] (
		path: Path, to: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends ActorFileAttr with Attributes(path, att, collectedAt) 
	
	case class OtherAtt private[Attributes] (
		path: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends Attributes(path,att,collectedAt)

}