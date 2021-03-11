package run.cosy.ldp.fs

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, scaladsl}
import akka.http.scaladsl.model.MediaTypes.{`text/plain`, `text/x-java-source`}
import akka.http.scaladsl.model.StatusCodes.{Created, InternalServerError}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Accept, Location, `Content-Type`}
import akka.stream.IOResult
import akka.stream.scaladsl.FileIO
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
	 * into the file `name`
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

	def mediaType(path: FPath): MediaType = MediaTypes.forExtension(extension(path))

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

class Resource(uri: Uri, linkName: FPath, context: ActorContext[AcceptMsg]) {
	import Resource.{CreateResource, mediaType}
	def behavior = FileData().start

	class FileData(variants: Set[FPath] = Set(), linkTo: Option[FPath] = None) {

		def linkToData: (FPath, FileData) =
			linkTo match
			case Some(link) => (link, FileData.this)
			case None =>
				val to: FPath = Files.readSymbolicLink(linkName)
				(to, new FileData(variants,Some(to)))

		def start: Behaviors.Receive[AcceptMsg] =
			Behaviors.receiveMessage[AcceptMsg] { (msg: AcceptMsg) =>
				import akka.http.scaladsl.model.HttpMethods.{GET, POST}
				msg match
					case cr @ CreateResource(linkToPath, Do(req, replyTo)) =>
						import cr.given
						val f: Future[IOResult] = req.entity.dataBytes.runWith(FileIO.toPath(linkToPath))
						f.andThen ({
							case Success(IOResult(count, _)) => replyTo ! HttpResponse(
								Created,
								Seq(Location(uri)),
								HttpEntity(`text/plain`.withCharset(HttpCharsets.`UTF-8`), s"uploaded $count bytes")
								)
							case Failure(e) =>
								// actually one may want to allow the client to continue from where it left off
								Files.deleteIfExists(linkToPath)
								replyTo ! HttpResponse(
									InternalServerError, Seq(),
									HttpEntity(`text/x-java-source`.withCharset(HttpCharsets.`UTF-8`), e.toString)
								)
						})
						//todo: here we should change the behavior to one where requests are stashed until the upload
						// is finished, or where they are returned with a "please wait" response...
						Behaviors.same
					case msg: Do =>
						msg.req.method match
							case GET => Get(msg.req, msg.replyTo)
							case _ => Behaviors.same

			}

		private def Get(req: HttpRequest, replyTo: ActorRef[HttpResponse]): Behavior[AcceptMsg] = {
			import akka.http.scaladsl.model.MediaTypes.`text/html`
			val (file, nextState) = linkToData
			val mt: MediaType = mediaType(file)
			val acceptHdrs = req.headers[Accept]
			if acceptHdrs.exists{ acc => acc.mediaRanges.exists(mr => mr.matches(mt)) } then {
				//todo: also check with the variants
				replyTo ! HttpResponse(
					StatusCodes.OK,
					Seq(), //todo: no headers for the moment
					HttpEntity.Default(
						ContentType(mt, () => HttpCharsets.`UTF-8`),
						linkName.toFile.length(),
						FileIO.fromPath(linkName)
					)
				)
			} else
				replyTo ! HttpResponse(
					StatusCodes.UnsupportedMediaType, Seq(),
					HttpEntity(`text/html`.withCharset(HttpCharsets.`UTF-8`),
						s"requested content Types where $acceptHdrs but we only have $mt")
				)
			nextState.start
		}
	}
}
