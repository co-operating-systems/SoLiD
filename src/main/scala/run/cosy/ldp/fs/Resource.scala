package run.cosy.ldp.fs

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, scaladsl}
import akka.http.scaladsl.model.MediaTypes.{`text/plain`, `text/x-java-source`}
import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Accept, Location, `Content-Type`}
import akka.stream.IOResult
import akka.stream.scaladsl.FileIO
import run.cosy.http.FileExtensions
import run.cosy.ldp.ResourceRegistry
import run.cosy.ldp.fs.BasicContainer.{Cmd, Do, Route}
import run.cosy.ldp.fs.Resource.connegNamesFor

import java.nio.file.{Files, Path => FPath}
import java.security.Principal
import scala.concurrent.Future
import scala.util.{Failure, Success}

//import akka.http.scaladsl.model.headers.{RequestClientCertificate, `Tls-Session-Info`}

/**
 * LDPR Actor extended with Access Control
 * In order to avoid having to list the whole directory to find out if a file exists,
 * we create a resource actor that can make a few cheaper spot checks for files given
 * the request, and so build up a little DB.
 *
 * A Resource can react to
 *  - GET and do content negotiation
 *  - PUT for changes
 *  - PATCH and QUERY potentially
 *
 * Because the Directory does not necessarily look if any of the resources exist, the actor
 * should close down soon after finding that there is nothing there.
 *
 * An LDPR actor should garbage collect after a time, to reduce memory useage
 */
object Resource {

	import Resource.CreateResource
	import HttpMethods._
	import StatusCodes._
	import akka.stream.Materializer

	import java.nio.file.Path
	import scala.concurrent.ExecutionContext

	//log.info(s"created LDPR($ldprUri,$path)")

	type AcceptMsg = Do | CreateResource
	
	/** send POST to child actor so that it can place the content body
	 * into the file `linkTo`
	 * (arguably one may want to only send the RequestBody, instead of the full Do)
	 **/
	case class CreateResource(
		linkTo: Path, cmd: Do
	)(using
	  val mat: Materializer, val ec: ExecutionContext
	)

	//guard could be a pool?
	//var guard: ActorRef = _

//  @throws[Exception](classOf[Exception])
//  def preStart() = {
//    log.info(s"starting guard for $ldprUri ")
//    guard = context.actorOf(Props(new Guard(ldprUri,List())))
//  }

	def mediaType(path: FPath): MediaType = FileExtensions.forExtension(extension(path))

	def extension(path: FPath) = {
		val file = path.getFileName.toString
		var n = file.lastIndexOf('.')
		if (n > 0) file.substring(n + 1)
		else ""
	}

	def apply(rUri: Uri, linkName: FPath, linkTo: FPath, name: String): Behavior[AcceptMsg] =
		Behaviors.setup[AcceptMsg] { (context: ActorContext[AcceptMsg]) =>
			//val exists = Files.exists(root)
//			val registry = ResourceRegistry(context.system)
//			registry.addActorRef(rUri.path, context.self)
//			context.log.info("started LDPR actor at " + rUri.path)
			new Resource(rUri, linkName, context).behavior
		}

	/** todo: language versions, etc...
	 * */
	def connegNamesFor(name: String, ct: `Content-Type`): List[String] =
		ct.contentType.mediaType.fileExtensions.map(name + "." + _)
}

import run.cosy.ldp.fs.Resource.AcceptMsg

class Resource(uri: Uri, linkPath: FPath, context: ActorContext[AcceptMsg]) {
	import Resource.{CreateResource, mediaType}
	import context.log

	log.info(s"starting Resource for <$uri> on $linkPath")

	def behavior = {
		val linkTo = Files.readSymbolicLink(linkPath)
		if linkTo.endsWith(".archive") then FileData().GoneBehavior
		else FileData().NormalBehavior
	}

	/**
	 * @param variants cache of variants for the resource - should be a more complex structure
	 * @param linkToOpt as an Opt because on creation the Linked to file may not yet have been written
	 */
	class FileData(val variants: Set[FPath] = Set(), val linkToOpt: Option[FPath] = None) {
		import akka.http.scaladsl.model.HttpMethods.{GET, POST, DELETE}
		import akka.http.scaladsl.model.StatusCodes.{Created, InternalServerError, Gone}

		def linkToData: (FPath, FileData) =
			linkToOpt match
			case Some(link) => (link, FileData.this)
			case None =>
				val to: FPath = Files.readSymbolicLink(linkPath)
				(to, new FileData(variants,Some(to)))

		// todo: Gone behavior may also stop itself earlier
		def GoneBehavior: Behaviors.Receive[AcceptMsg] =
			Behaviors.receiveMessage[AcceptMsg] { (msg: AcceptMsg) =>
				msg match
				case Do(req, replyTo) => replyTo ! HttpResponse(Gone)
				case CreateResource(_, Do(req, replyTo)) =>
					log.warn(s"received Create on resource <$uri> at <$linkPath> that has been deleted! " + 
						s"Message should not reach this point.")
					replyTo ! HttpResponse(InternalServerError,entity=HttpEntity("please contact admin"))
				Behaviors.same
			}

		def NormalBehavior: Behaviors.Receive[AcceptMsg] =
			Behaviors.receiveMessage[AcceptMsg] { (msg: AcceptMsg) =>
				msg match
				case cr @ CreateResource(linkToPath, Do(req, replyTo)) =>
					log.info(
						s"""received POST request with headers ${req.headers} and CT=${req.entity.contentType} 
							|Saving to: $linkToPath""".stripMargin)
					import cr.given
					val f: Future[IOResult] = req.entity.dataBytes.runWith(FileIO.toPath(linkToPath))
					f.andThen ({
						case Success(IOResult(count, _)) => replyTo ! HttpResponse(
							Created,
							Seq(Location(uri)),
							HttpEntity(`text/plain(UTF-8)`, s"uploaded $count bytes")
							)
						case Failure(e) =>
							log.warn(s"Unable to prcess request $cr. Deleting $linkToPath")
							// actually one may want to allow the client to continue from where it left off
							Files.deleteIfExists(linkToPath)
							replyTo ! HttpResponse(
								InternalServerError, Seq(),
								HttpEntity(`text/x-java-source`.withCharset(`UTF-8`), e.toString)
							)
					})
					//todo: here we should change the behavior to one where requests are stashed until the upload
					// is finished, or where they are returned with a "please wait" response...
					Behaviors.same
				case Do(req, replyTo) =>
					req.method match
						case GET => Get(req, replyTo)
						case DELETE => Delete(req, replyTo) 
			}
		end NormalBehavior

		private def Get(req: HttpRequest, replyTo: ActorRef[HttpResponse]): Behavior[AcceptMsg] = 
			import akka.http.scaladsl.model.ContentTypes.`text/html(UTF-8)`
			import akka.http.scaladsl.model.MediaTypes.`text/html`
			val (file, nextState) = linkToData
			val mt: MediaType = mediaType(file)
			val acceptHdrs = req.headers[Accept]
			if acceptHdrs.exists{ acc => acc.mediaRanges.exists(mr => mr.matches(mt)) } then 
				//todo: also check with the variants
				replyTo ! HttpResponse(
					StatusCodes.OK,
					Seq(), //todo: no headers for the moment
					HttpEntity.Default(
						ContentType(mt, () => HttpCharsets.`UTF-8`),
						linkPath.toFile.length(),
						FileIO.fromPath(linkPath)
					))
			else
				replyTo ! HttpResponse(
					StatusCodes.UnsupportedMediaType, 
					entity=HttpEntity(`text/html(UTF-8)`,
						s"requested content Types where $acceptHdrs but we only have $mt"
					))
			nextState.NormalBehavior
		end Get

		/**
		 * Specified in:
		 *  - [[https://tools.ietf.org/html/rfc7231#section-4.3.5 4.3.5 DELETE]] of HTTP 1.1 RFC 7231 
		 *  - [[https://www.w3.org/TR/ldp/#ldpc-HTTP_DELETE 5.2.5 HTTP DELETE]] of LDP spec
		 *  - [[https://solidproject.org/TR/protocol#deleting-resources in 5.4 Deleting Resources]] of Solid Protocol
		 *  
		 * Creates a "file.archive" directory, moves the link into it and points the link to the archive.
		 * Then it can return a response. And the done behavior is started after cleanup.
		 *  
		 * Should also delete Auxilliary resources (which at present just include the linked-to document)
		 * When done the actor must shutdown and send a deleted notification to the parent, so that it
		 * can remove the link from its cache.
		 * 
		 * what should one do if a delete fails on some of the resources?
		 * Best to create an archive directory and move all files there, so that they are no longer visible, 
		 * and can be cleaned away later.
		 *
		 * 
		 * @param req the DELETE Request
		 * @param replyTo
		 * @return
		 */
		def Delete(req: HttpRequest, replyTo: ActorRef[HttpResponse]): Behavior[AcceptMsg] =
			import java.nio.file.StandardCopyOption

			import java.nio.file.LinkOption.NOFOLLOW_LINKS
			import akka.http.scaladsl.model.StatusCodes.{Conflict, NoContent}

			import java.io.IOException
			import java.nio.file.attribute.BasicFileAttributes
			import java.nio.file.{DirectoryNotEmptyException, FileAlreadyExistsException, NoSuchFileException, NotLinkException,AtomicMoveNotSupportedException}

			val javaMime = `text/x-java-source`.withCharset(`UTF-8`)
			val linkTo: FPath = try {
				val lp = Files.readSymbolicLink(linkPath)
				linkPath.resolveSibling(lp)
			} catch {
				case e: UnsupportedOperationException => 
					log.error("Operating Systems does not support links.",e)
					replyTo ! HttpResponse(InternalServerError, entity=HttpEntity(`text/plain(UTF-8)`,"Server error"))
					//todo: one could avoid closing the whole system and instead shut down the root solid actor (as other
					// parts of the actor system could continue working
					context.system.terminate()
					return Behaviors.stopped
				case e: NotLinkException => 
					log.warn(s"type of <$linkPath> changed since actor started",e)
					replyTo ! HttpResponse(Conflict, entity=HttpEntity(`text/plain(UTF-8)`,
						"Resource seems to have changed type"))
					return Behaviors.stopped
				case e : SecurityException => 
					log.error(s"Could not read symbolic link <$linkPath> on DELETE request", e)
					//todo: returning an InternalServer error as this is not expected behavior
					replyTo ! HttpResponse(InternalServerError, entity=HttpEntity(`text/plain(UTF-8)`, 
						"There was a problem deleting resource on server. Contact Admin."))
					return Behaviors.stopped
				case e: IOException => 
					log.error(s"Could not read symbolic link <$linkPath> on DELETE request. " +
						s"Will continue delete attempt",e)
					linkPath
			}
			val archive = try {
				Files.createDirectory(archiveDir)
			} catch {
				case e: UnsupportedOperationException =>
					log.error("Operating Systems does not support creation of directory with no(!) attributes!" +
						" This should never happen", e)
					replyTo ! HttpResponse(InternalServerError, entity=HttpEntity(`text/plain(UTF-8)`,"Server error"))
					//todo: one could avoid closing the whole system and instead shut down the root solid actor (as other
					// parts of the actor system could continue working
					context.system.terminate()
					return Behaviors.stopped
				case e: FileAlreadyExistsException =>
					log.warn(s"Archive directory <$archiveDir> allready exists. " +
						s"This should not have happened. Logic error in program suspected. " +
						s"Or user created archive dir by hand. " +
						s"Will continue by trying to save to existing archive. Things could go wrong.", e)
					archiveDir	
				case e: SecurityException => 
					log.error(s"Exception trying to create archive directory. " +
						s"JVM is potentially badly configured. ", e)
					replyTo ! HttpResponse(InternalServerError, entity=HttpEntity(`text/plain(UTF-8)`,
						"Server error. Potentially badly configured."))
					return Behaviors.stopped
				case e: IOException =>  
					log.error(s"Could not create archive dir <$archiveDir> for DELETE", e)
					replyTo ! HttpResponse(InternalServerError, entity=HttpEntity(`text/plain(UTF-8)`, "could not delete, contact admin"))
					return Behaviors.stopped
			}
			
			try { //move link to archive. At the minimum this will result in a delete
				Files.move(linkPath, archive.resolve(linkPath.getFileName))
			} catch {
				case e: UnsupportedOperationException => // we don't even have a symlink. Problem!
					log.error(s"tried move <$linkPath> to <$archive>. " +
						s"But OS does not support an attribute?. JVM badly configured. ",e)
					replyTo ! HttpResponse(InternalServerError,
						entity=HttpEntity(`text/plain(UTF-8)`,"Server badly configured. Contact admin."))
					return Behaviors.stopped
				case e: DirectoryNotEmptyException =>
					log.error(s"tried move link <$linkPath> to <$archive>. " +
						s" But it turns out the moved object is a non empty directory!",e)
					replyTo ! HttpResponse(Conflict, entity=HttpEntity(`text/plain(UTF-8)`,
						"Coherence problem with resources. Try again."))
					return Behaviors.stopped
				case e: AtomicMoveNotSupportedException => 
					log.error(s"Message should not appear as Atomic Move was not requested. Corrupt JVM?",e)
					replyTo ! HttpResponse(InternalServerError,
						entity=HttpEntity(`text/plain(UTF-8)`,"Server programming or installation error"))
					return Behaviors.stopped
				case e: SecurityException =>
					log.error(s"Security Exception trying move link <$linkPath> to archive.",e)
					replyTo ! HttpResponse(InternalServerError,
						entity=HttpEntity(`text/plain(UTF-8)`,"Server programming or installation error"))
					return Behaviors.stopped
				case e: IOException => 
					log.error(s"IOException trying move link <$linkPath> to archive.",e)
					replyTo ! HttpResponse(InternalServerError,
						entity=HttpEntity(`text/plain(UTF-8)`,"Server programming or installation error"))
					return Behaviors.stopped
				case e: FileAlreadyExistsException => 
					log.warn(s"trying to move <$linkPath> to <$archive> but a file with that name already exists. " +
						s"Will continue with DELETE operation.")
					try {
						Files.delete(linkPath)
					} catch {
						case e: NoSuchFileException => 
							log.warn(s"Trying to delete <$linkPath> link but it no longer exists. Will continue DELETE operation")
						case e: DirectoryNotEmptyException =>
							log.warn(s"Attempting to delete <$linkPath> link but it suddenly " +
								s" seems to have turned into a non empty directory! ", e)
							replyTo ! HttpResponse(Conflict,
								entity=HttpEntity(`text/plain(UTF-8)`,"Coherence problem with resources. Try again."))
							return Behaviors.stopped
						case e: SecurityException =>
							log.error(s"trying to delete <$linkPath> caused a security exception. " +
								s"JVM rights not properly configured.",e)
							replyTo ! HttpResponse(InternalServerError,
								entity=HttpEntity(`text/plain(UTF-8)`,"Server badly configured. Contact admin."))
							return Behaviors.stopped
						case e : IOException =>
							log.error(s"trying to delete <$linkPath> link cause IO Error." +
								s"File System problem.",e)
							replyTo ! HttpResponse(InternalServerError,
								entity=HttpEntity(`text/plain(UTF-8)`,"Problem on server. Contact admin."))
							return Behaviors.stopped
					}
			}
			try {
				Files.createSymbolicLink(linkPath,archiveDir)
			} catch {
				case e => 
					log.warn(s"Could not create link to Archive directory <$linkPath> -> <$archiveDir>. " +
						s"Could lead to problems!",e)
			}
			
			//if we got this far we got the main work done. So we can always already return a success
			replyTo ! HttpResponse(NoContent)
			
			//todo: here we should actually search the file system for variants
			//  this could also be done by a cleanup actor
			// Things that should not be cleaned up:
			//   - <file.count> as that gives the count for the next version.
			//   - other ?
			//  This indicates that it would be good to have a link to the archive
			val cleanup: List[FPath] = if linkPath == linkTo then variants.toList else linkTo::variants.toList
			cleanup.foreach{ p => 
				try {
					Files.move(p, archive.resolve(p.getFileName))
				} catch {
					case e: UnsupportedOperationException => // we don't even have a symlink. Problem!
						log.error(s"tried move <$p> to <$archive>. " +
							s"But OS does not support some attribute? JVM badly configured. ",e)
					case e: FileAlreadyExistsException =>
						log.warn(s"trying to move <$p> to <$archive> but a file with that name already exists. " +
							s"Will continue with DELETE operation.")
					case e: DirectoryNotEmptyException =>
						log.error(s"tried move link <$p> to <$archive>. " +
							s" But it turns out the moved object is a non empty directory!",e)
					case e: AtomicMoveNotSupportedException =>
						log.error(s"Tried moving <$p> to <$archive>. " +
							s"Message should not appear as Atomic Move was not requested. Corrupt JVM?",e)
					case e: SecurityException =>
						log.error(s"Security Exception trying move link <$p> to archive.",e)
					case e: IOException =>
						log.error(s"IOException trying move link <$p> to archive.",e)
				}
			}
			this.GoneBehavior
		end Delete

		/** Directory where an archive is stored before deletion */
		def archiveDir: FPath = linkPath.resolveSibling(linkPath.getFileName.toString + ".archive")


	}
}
