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

	import java.nio.file.Paths
	import java.nio.file.attribute.FileTime

	sealed trait Attributes(path: Path, att: BasicFileAttributes, collectedAt: Instant)

	def apply(fileName: Path, att: BasicFileAttributes, 
	          collectedAt: Instant = Instant.now()): Attributes = 
		if (att.isDirectory) DirAtt(fileName,att,collectedAt)
		else if (att.isSymbolicLink) {
			val linkTo = Files.readSymbolicLink(fileName)
			if linkTo.endsWith(".archive") then
				Archived(fileName, att, collectedAt)
			else SymLink(fileName, linkTo, att, collectedAt)
		}
		else OtherAtt(fileName,att,collectedAt)

	/** Fails with the exceptions from Files.readAttributes() */
	def forPath(path: Path): Try[Attributes] =
		import java.nio.file.LinkOption.NOFOLLOW_LINKS
		import java.nio.file.attribute.BasicFileAttributes
		Try {
			val att = Files.readAttributes(path, classOf[BasicFileAttributes], NOFOLLOW_LINKS)
			Attributes(path,att)
		}
	
	def createDir(inDir: Path, dirName: String): Try[DirAtt] = Try {
		val path = inDir.resolve(dirName)
		val newDirPath = Files.createDirectory(path)
		val now = Instant.now()
		val ftNow = FileTime.from(now)
		DirAtt(newDirPath,
			new BasicFileAttributes {
				override def lastModifiedTime(): FileTime = ftNow
				override def lastAccessTime(): FileTime = ftNow
				override def creationTime(): FileTime = ftNow
				override def isRegularFile: Boolean = false
				override def isDirectory: Boolean = true
				override def isSymbolicLink: Boolean = false
				override def isOther: Boolean = false
				override def size(): Long = 0
				override def fileKey(): AnyRef = null
			},
			now
		)
	}

	/**
	 * Create a Symbolic Link
	 * @param dirPath  the path of the directory in which the link will be placed
	 * @param linkName the name of the symbolic link
	 * @param linkTo the relative name of the file linked to
	 * @return The full path to the symbolic link relative to the base where the JVM is running
	 */
	def createLink(dirPath: Path, linkName: String, linkTo: String): Try[SymLink] = Try {
		import java.nio.file.Paths
		import java.nio.file.attribute.FileTime
		val path = dirPath.resolve(linkName)
		val linkPath = Files.createSymbolicLink(path, Paths.get(linkTo))
		val now = Instant.now()
		val ftNow = FileTime.from(now)
		//avoid checking the file system again, create the obvious answer
		SymLink(linkPath,Paths.get(linkTo),
			new BasicFileAttributes {
				import java.nio.file.attribute.FileTime
				override def lastModifiedTime(): FileTime =  ftNow
				override def lastAccessTime(): FileTime = ftNow
				override def creationTime(): FileTime = ftNow
				override def isRegularFile: Boolean = false
				override def isDirectory: Boolean = false
				override def isSymbolicLink: Boolean = true
				override def isOther: Boolean = false
				override def size(): Long = 0
				override def fileKey(): AnyRef = null
			},now)
	}

	//todo: may want to remove this later. Just here to help me refactor code
	sealed trait ActorFileAttr extends Attributes
	sealed trait Other extends Attributes
	
	case class DirAtt private[Attributes] (
		path: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends ActorFileAttr with Attributes(path,att,collectedAt)

	case class SymLink private[Attributes] (
		path: Path, to: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends ActorFileAttr with Attributes(path, att, collectedAt) 
	
	case class OtherAtt private[Attributes] (
		path: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends Other with Attributes(path,att,collectedAt)
	
	case class Archived private[Attributes] (
		path: Path, att: BasicFileAttributes, collectedAt: Instant
	) extends Other with Attributes(path,att,collectedAt)

}