package run.cosy.ldp.fs

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, PreRestart}
import akka.http.scaladsl.model
import akka.http.scaladsl.model.StatusCodes.{InternalServerError, MovedPermanently, NotFound, NotImplemented, OK}
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.http.scaladsl.server.ContentNegotiator.Alternative
import akka.http.scaladsl.server.ContentNegotiator.Alternative.ContentType
import akka.http.scaladsl.server.{RequestContext, RouteResult}
import akka.stream.scaladsl.{FileIO, RunnableGraph, Source}
import akka.stream.{ActorMaterializer, IOResult, Materializer}
import akka.util.ByteString
import akka.{Done, NotUsed}
import cats.data.NonEmptyList
import run.cosy.http.Headers.Slug
import run.cosy.ldp

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import java.lang.UnsupportedOperationException
import java.nio.file.attribute.UserDefinedFileAttributeView
import java.nio.file.{FileAlreadyExistsException, FileSystem}
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZoneOffset}
import java.util.{Locale, stream}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try, Using}
//import run.cosy.http.Slug

import akka.event.slf4j.Slf4jLogger
import run.cosy.ldp.fs.BasicContainer
import run.cosy.ldp.fs.BasicContainer.Cmd
import run.cosy.ldp.ResourceRegistry

import java.nio.file.{Files, Path}
import scala.collection.immutable.HashMap
import scala.util.Random

/**
 * An LDP/Solid Container corresponding to a directory on the FileSystem.
 *
 * Because we are typing the actor, we need to check before creation what the type of the actor 
 * created is!! The main difference between a directory and a file is that a directory can route
 * messages to subdirectores (hence `Route`` and `Do`). The directory is in a good position 
 * to know its contents. 
 *
 * If the next message is a `Route` then depending on the action
 * - PUT: must create the container
 * - GET, POST, DELETE: must be sent to another container
 *
 * If the next message is a `Do` then it needs to check
 * * If the URL ends in `/` the next actor must be mapped to a directory
 * * Otherwise the container needs to look at the mime type and calculate the resource
 * e.g. `cat` with mime type `image/jpg` goes to `cat.jpg`
 * e.g. `cat.jpg` with mimte type `image/jpg` got to `cat.jpg`
 * e.g. `cat.exe` with mime type `image/jpg` goes to `cat.exe$.jpg`
 * * POST must create a new name for the resource if it already exists and forward that to the actor
 * * GET, PUT, ... can create the resource actor which will then copy the content to disk or serve it
 *
 * Note: because actors have unique names, the container only needs to create the actor and forward the message.
 * It does not need to block on saving to disk, or creating the file. 
 *
 * If we did not type the actor then the container could for GET just create the actor,  which could
 * check its existence. 
 *
 * Note, the container may need to read some config files on startup, so it will need to
 * check its own existence in any case.
 *
 * It is the Container's role to 
 *  - name new resources on a POST, and check with the Guard they can be created
 *  - on a PUT with intermediary dirs it can create the subactor, which will create the resource,
 *    this works because actors have unique names.  But the container needs to check with its Guard 
 *    before doing that.
 *  - on DELETE it needs to check with the container and the actor
 *  - route messages for GET onto subactors (which have a guard?) 
 *  - for DIRS these get passed along 
 *  - for files these get passed along 
 *  - PATCH is like PUT
 *   - QUERY is like GET
 * */
object BasicContainer {

	import java.time.{Clock, Instant}

	/** A collection of "unwise" characters according to [[https://tools.ietf.org/html/rfc2396#section-2.4.3 RFC 2396]]. */
	val UnwiseChars = """{}|\^[]`"""

	/** A collection of "invalid" characters according to [[https://tools.ietf.org/html/rfc2396#section-2.4.3 RFC 2396]]. */
	val Delims = """"<>#%""""

	/** Generic delimiters according to [[https://tools.ietf.org/html/rfc3986#section-2.2 RFC 3986]] */
	val GenDelims = """:/?#[]@"""
	val SubDelims = """!$&'()*+,;="""
	val ReactiveSolidDelims = "_"
	val Remove = (UnwiseChars + Delims + GenDelims + SubDelims + ReactiveSolidDelims).toSet

	val MaxFileName = 100

	def santiseSlug(slugTxt: String): String =
		val santized: String = slugTxt.takeWhile(_ != '.').filterNot(c => Remove.contains(c) || c.isWhitespace)
		santized.substring(0, Math.min(MaxFileName, santized.size))

	val timeFormat = DateTimeFormatter.ofPattern("yyyyMMdd-N").withZone(ZoneId.of("UTC"))
	def createTimeStampFileName(clock: Clock): String =  timeFormat.format(LocalDateTime.now(clock))

	//todo: add Content-Encoding, Content-Language
	def linkToName(linkName: String, cts: ContentType) =
		linkName +"."+cts.contentType.mediaType.fileExtensions.headOption.getOrElse("bin")

	/** @return (linkName, linkTo) strings */
	def createName(req: HttpRequest, clock: Clock = Clock.systemDefaultZone): (String, String) = {
		val linkName = req.headers.collectFirst{case Slug(name) => name}
			.map(slug => santiseSlug(slug.toString))
			.getOrElse(createTimeStampFileName(clock))
		val linkTo = linkToName(linkName, req.entity.contentType)
		(linkName, linkTo)
	}

	// log.info(s"created LDPC($ldpcUri,$root)")

	def apply(
		containerUrl: Uri,
		dir: Path
	)(
		using
		reg: ResourceRegistry
	): Behavior[Cmd] = Behaviors.setup[Cmd] { (context: ActorContext[Cmd]) =>
		//val exists = Files.exists(root)
		reg.addActorRef(containerUrl.path, context.self)
		context.log.info("started actor at " + containerUrl.path)
		new BasicContainer(containerUrl, dir, new Spawner(context)).behavior
	}

	sealed trait Cmd

	sealed trait ReqCmd extends Cmd {
		val req: HttpRequest
		val replyTo: ActorRef[HttpResponse]
	} 

	/** @param path the path to the final resource */
	final case class Route(
		path: NonEmptyList[String],
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqCmd {
		def name: String = path.head

		// check that path is not empty before calling  (anti-pattern)
		def next: ReqCmd = path match
			case NonEmptyList(head, Nil) => Do(req, replyTo)
			case NonEmptyList(head, tail) => Route(NonEmptyList(tail.head, tail.tail), req, replyTo)
	}

	/** a command to be executed on the resource on which it arrives */
	final case class Do(
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqCmd


	// responses from child to parent
	case class ChildTerminated(name: String) extends Cmd


}

/**
 * Class of Resource objects, mapped to a path on the file system.
 * Our convetion for files on the FS at present (later to be modularized) is designed
 * to avoid needing to load the container contents into memory, and to allow content negotiation
 * to work both locally and on the file system. These are the rules:
 *  1. The root of the file is the string up to the first "."(directories don't have that restriction)
 *  1. All roots have a symbolic link to the original resource. So if `cat.raw` is a large picture loaded first
 *     then `cat` is a symbolic link to `cat.raw`. Later the resource actor can keep copies of other versions 
 *     for other devices such as smart phones, but link will keep track of which is the original file.
 *  1. Requests for `cat.jpg` are passed to the `cat` child actor that can deal with content negotiation for that
 *    resource
 *  1. Languagee based content negotiation (not implemented) would on a request for `blog` search for `blog.en.html`
 *  1. The file pointed to by the symlink will contain metadata (perhaps extended attributes) informing the server of other versions. 
 *  1. The directory will contain xattrs of its own: 
 *    - one of these will be set after the actor has gone through the directory to set all the symlinks, so it knows
 *      that it need not do it again
 *    - others?
 *  1. To be able to deal with Multiple Posts of content with the same Slug (Eg. If a POST with `Slug: My Blog` 
 *     is posted 3 times) the container needs to know what the next version is without having to download the whole dir.
 *     We can add metadata for a symlink so that the container can know that the next name should be MyBlog_4, 
 *     without needing to search for MyBlog_1`, `MyBlog_2` and `MyBlog_3`
 *     This avoids having to create ugly hash based file names.
 *  1. alternatively instead of using xattrs every root could come with a `x.meta.ttl` that contains the file
 *     meta-data
 *
 *  Some Advantages:
 *  
 *  - The symbolic link trick allows Content Negotiation to  work correctly when files are edited on the FS direectly,
 *  since links with relative URLs in one document to the root name of a file, can be viewed locally as if they 
 *  were Negotiated by local editors, since they can find the file by following the symbolic link.
 *  - It also helps a lot for RDF formats: If the first version uploaded is `card.ttl` then `card` will link to `card.ttl`
 *  and that file can be the one updated on PATCH or PUT. 
 *  - We want to make Content Negotiation work right out of the box, so as to avoid changes in format preferences
 *    (such as a move from ttl to json-ld or vice versa, breaking all the links)
 *  - There are important speed advantage to be had, by using xattrs as that avoids walking the whole directory 
 *     - an actor only needs to check if the root of the file (e.g. `blog`) exists, which is one call to the FS. If the file exists, then the Container can start the child actor, and later pass on calls to it
 *     - The child actor can retrieve the Xattrs from the original file, to find out where the variants are.
 *     Otherwise the Containere would need to load the whole directory contents in memory, which could be slow if too many
 *     files are listed there, and the result would uses up a lot of memory. (this may be negligeable for a few directores,
 *     but could become problematic for a large number of open ones).
 *    
 *  Notes: 
 *    - the actor could load the full dir data into memory on a container that is heavily used when needed. 
 *    - ideally the attributes should be mapped to the files, so that it should be possible to recover them by
 *      running through the files
 *
 * @param containerUrl the URL of the container. (without the slash, so that it is east to write `ctnr / path`) 
 * @param dirPath      the path on the filesystem that corresponds to the container
 */
class BasicContainer private(
	containerUrl: Uri,
	dirPath: Path,
	context: Spawner
)(
	using
	reg: ResourceRegistry
) {

	import BasicContainer.{Do, ReqCmd, Route, linkToName, santiseSlug}
	import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
	import akka.http.scaladsl.model.MediaTypes.{`text/plain`, `text/x-java-source`}
	import akka.http.scaladsl.model.headers.{Link, LinkParams, LinkValue, Location, `Content-Type`}
	import akka.http.scaladsl.model.{ContentTypes, HttpCharsets, HttpEntity, HttpMethods}
	import HttpMethods.{GET, POST}
	import Resource.CreateResource
	import Attributes.ActorFileAttr
	import run.cosy.ldp.fs.{Ref,CRef,RRef}
	
	import java.nio.file.attribute.BasicFileAttributes
	import java.time.Instant
	//Cashed Contents of Directory 
	type Contents = HashMap[String,Ref|ActorFileAttr]
	type Counter  = Map[String, Int]
	// one could add a behavior for if it exists but is not a container

	// we don't need this yet, as we are not shutting down the Container Actor yet.
	//			  .receiveSignal {
	//				case (ctx, PostStop) =>
	//					registry.removePath(ldpcUri.path)
	//					Behaviors.stopped
	//			}

	def behavior: Behavior[Cmd] = {
		val isDir = Files.isDirectory(dirPath)
		if (isDir) new Dir().start
		else ??? //if PUT create dir else return error
	}



	// url for resource with `name` in this container
	def urlFor(name: String): Uri = containerUrl.withPath(containerUrl.path / name)


//	def notContainerBehavior(exists: Boolean): Behaviors.Receive[Cmd] = 
//		val exists = Files.exists(dirPath)
//		Behaviors.receiveMessage[Cmd] { msg =>
//			msg match
//				case Route(path, HttpRequest(PUT, _, _, _, _), replyTo) => 
//					replyTo ! HttpResponse(MethodNotAllowed, entity = s"PUT not implemented")
//				case Route(_, req,replyTo) => replyTo ! HttpResponse(NotFound, entity = s"could not find ${req.uri}")
//				case Do(req,replyTo) => replyTo ! HttpResponse(MethodNotAllowed, entity = s"Not implemented")
//			Behaviors.same
//		}

	//the dir behavior caches info about the ldp:contains relations and about counters for files
	//to avoid requests to the FS
	class Dir(contains: Contents = HashMap(), counters: Counter = HashMap()) {

		import java.nio.file.attribute.BasicFileAttributes
		import java.time.Instant

		lazy val start: Behavior[Cmd] = {
			Behaviors.receiveMessage[Cmd] { (msg: Cmd) =>
				import BasicContainer.ChildTerminated
				msg match
					case act: Do => run(act)
					case routeMsg: Route => routeHttpReq(routeMsg)
					case ChildTerminated(name) =>
						reg.removePath(containerUrl.path / name)
						new Dir(contains - name,counters).start
			}.receiveSignal {
				case (_, signal) if signal == PreRestart || signal == PostStop =>
					counters.foreach { (name, int) =>
						saveSiblingsCountFor(name, int)
					}
					Behaviors.same
			}
		}
		
		/**
		 * Get Ref for name by checking in cache, and if not on the FS. It does not create an actor for the Ref.
		 * (An improved version could decide not to check the FS if a full upload of the FS properties have just
		 * occurred.)
		 *
		 * @prefix name the content negotiated name of the resource and actor relative to this Dir
		 * @prefix createActorIfNeeded if the resource has not actor associated with it, created it first
		 * @return the Ref and the new Dir state, or None of there is no Reference at that location.
		 * */
		def getRef(name: String): Option[(Ref, Dir)] =
			contains.get(name).map { (v : Ref|ActorFileAttr)  =>
				import Attributes.{DirAtt, SymLink}
				v match 
				case ref: Ref => (ref, Dir.this)
				case fa: ActorFileAttr  =>
					val r = context.spawn(fa,urlFor(name))
					(r, new Dir(contains + (name -> r), counters))	
			}.orElse {
				import java.io.IOException
				import java.nio.file.LinkOption.NOFOLLOW_LINKS
				import java.nio.file.attribute.BasicFileAttributes
				val path = dirPath.resolve(name)  //todo: also can throw exception
				Attributes.forPath(path) match
					case Success(att: ActorFileAttr) =>
						val r: Ref = context.spawn(att,urlFor(name))
						Some((r, new Dir(contains + (name -> r), counters)))
					case Failure(err) => 
						err match
						case e: UnsupportedOperationException =>
							context.log.error(s"Cannot get BasicFileAttributes! JDK misconfigured on request <$path>", e)
							None
						case e: IOException =>
							context.log.info(s"IOException trying to get <$path>", e)
							None
						case e: SecurityException =>
							context.log.warn(s"Security Exception on searching for attributes on <$path>.", e)
							None
					case _ => None
			}
		end getRef

				
		// state monad	fn. create symlink to new file (we don't have an actor for it yet)
		def createSymLink(linkName: String, linkTo: String): Try[(RRef, Dir)] =
			Attributes.createLink(dirPath, linkName,linkTo).map{ att =>
				val ref = context.spawn(att,urlFor(linkName))
				(ref, new Dir(contains + (linkName -> ref),counters))
			}
			

			
		/** state monad fn	- return counter for given base name so that one can create the next name 
		 * e.g. for base name `cat`  and number 3 the next created file could be `cat_3` */ 
		def nextCounterFor(baseName: String): (Int, Dir) =
			val newcounter: Int = findSiblingCount(baseName)+1
			(newcounter, new Dir(contains, counters + (baseName -> newcounter)))
			
		def countFile(countFileRoot: String): java.io.File = dirPath.resolve(countFileRoot + ".count").toFile
		
		/** You need to update the counter if you use it. 
		 *  This suggests one should have monadic interface on the config data */
		def findSiblingCount(countFileRoot: String): Int = { 
			counters.get(countFileRoot).getOrElse {
				val cf = countFile(countFileRoot)
				Using(new BufferedReader(new FileReader(cf))) { (reader: BufferedReader) =>
					reader.readLine().toInt
				}.recover {
					case e : java.io.FileNotFoundException => cf.createNewFile(); 1
					case e : java.io.IOException => context.log.warn(s"Can't read count file <$cf>", e); 1
					case e : java.lang.NumberFormatException => context.log.warn(s"error parsing counter in <$cf>"); 1
				}.getOrElse(0)
			}
		}

		//todo: save such info to disk on shutdown
		def writeSiblingCount(countFileRoot: String, count: Int): Counter = counters + (countFileRoot -> (count+1))
		
		def saveSiblingsCountFor(name: String, count: Int): Unit = 
			Using(new BufferedWriter(new FileWriter(countFile(name)))) { (writer: BufferedWriter) =>
				writer.write(count.toString)
			} recover {
				case e => context.log.warn(s"Can't save counter value $count for <$countFile>", e)
			}


		protected
		def run(msg: Do): Behavior[Cmd] = 
			import akka.stream.alpakka.file.scaladsl.Directory
			import msg.{ec, mat, req}
			import req._
			if (!uri.path.endsWithSlash)
				//this should have been dealt with by parent container, which should have tried conneg.
				val ldpcUri = uri.withPath(uri.path / "")
				msg.replyTo ! HttpResponse(
					MovedPermanently, Seq(Location(ldpcUri)), entity = s"This resource is now a container at ${uri}"
				)
				Behaviors.same
			else method match
				case GET => //return visible contents of directory
					msg.replyTo ! HttpResponse(
						OK, Seq(),
						HttpEntity(ContentTypes.`text/plain(UTF-8)`, 
							Directory.ls(dirPath).map(p => ByteString(p.toString + "\n")))
					)
					Behaviors.same
				case POST =>  //create resource
					//todo: create sub container for LDPC Post
					import BasicContainer.createName
					val (linkName: String, linkTo: String) = createName(msg.req)
					val response: Try[(RRef, Dir)] = createSymLink(linkName,linkTo).recoverWith {
						case e : FileAlreadyExistsException =>   // this is the pattern of a state monad!
							val (nextId, newDir) = nextCounterFor(linkName)
							val nextLinkName = linkName+"_"+nextId
							newDir.createSymLink(nextLinkName,linkToName(nextLinkName,entity.contentType))
					}
					response match {
						case Success((ref, dir)) =>
							ref.actor ! CreateResource(dirPath.resolve(ref.att.to),msg)
							dir.start
						case Failure(e) => 
							HttpResponse(InternalServerError, Seq(),
								HttpEntity(`text/x-java-source`.withCharset(HttpCharsets.`UTF-8`), e.toString) )
							Behaviors.same
					}
				//todo: create new PUT request and forward to new actor?
				//or just save the content to the file?
				case _ =>
					HttpResponse(NotImplemented, Seq(), entity = s"have not implemented  ${msg.req.method} for ${msg.req.uri}")
					Behaviors.same
		end run

		protected
		def routeHttpReq(msg: Route): Behavior[Cmd] = {
			context.log.info(
				s"received msg: ${msg.req.uri} in ${dirPath.toFile.getAbsolutePath} in container with uri $containerUrl")

			msg.next match
				case doit: Do =>
					if doit.req.uri.path.endsWithSlash then
						forwardToContainer(msg.name, msg)
					else forwardMsgToResourceActor(msg.name, doit)
				case route: Route => forwardToContainer(msg.name, msg)
		}

		//todo: do we need a name cleanup happen? Perhaps we don't need that for containers?
		//  but getRef can also return a RRef... So `cat.jpg` 
		//  here would return a `cat.jpg` RRef rather than `cat` or a `CRef`
		//  this indicates that the path name must be set after checking the attributes!
		def forwardToContainer(name: String, msg: ReqCmd): Behavior[Cmd] = 
			getRef(name) match 
				case Some((ref,dir)) =>
					ref match
					case CRef(att, actor) => actor ! msg
					case RRef(att, actor) => // there is no container, so redirect to resource
						msg match
							case Do(req, replyTo) =>  //we're at the path end, so we can redirect
								val uri = msg.req.uri
								val redirectTo = uri.withPath(uri.path.reverse.tail.reverse)
								replyTo ! HttpResponse(MovedPermanently, Seq(Location(redirectTo)))
							case Route(path, req, replyTo) => // the path passes through a file, so it must end here
								replyTo ! HttpResponse(NotFound, Seq(), s"Resource with URI ${req.uri} does not exist")
					dir.start
				case None => { msg.replyTo ! HttpResponse(NotFound, Seq(),
					HttpEntity(`text/plain`.withCharset(`UTF-8`),
							s"""Resource with URI ${msg.req.uri} does not exist."""))
					Behaviors.same }
		end forwardToContainer
		
		
		/**
		 * Forward message to child resource (not container)
		 * @param name Content-Negotiation root name: e.g. `cat` can return `cat.jpg` or `cat.png`
		 * @param msg the HttpRequest
		 * @return A new Behavior, updated if a new child actor is created.
		 */
		protected
		def forwardMsgToResourceActor(name: String, msg: Do): Behavior[Cmd] =
			val dotLessName = actorNameFor(name)
			val obj = getRef(dotLessName)
			obj match
				case Some((ref,dir)) => {
					ref match {
						case CRef(att, actor) => msg.replyTo ! {
							if (dotLessName == name)
								val uri = msg.req.uri
								HttpResponse(MovedPermanently, Seq(Location(uri.withPath(uri.path / ""))))
							else HttpResponse(NotFound)
						}
						case RRef(att, actor) => actor ! msg
						case null => context.log.warn(s"did not match anthing in $dirPath: received ref ="+ref)
					}
					dir.start
				}
				case None => msg.replyTo ! HttpResponse(NotFound,
					entity = HttpEntity(`text/plain`.withCharset(`UTF-8`),
						s"""Resource with URI ${msg.req.uri} does not exist. 
							|Try posting to <${containerUrl}> container first.""".stripMargin))
					Behaviors.same
		end forwardMsgToResourceActor
		
		/**
		 * Given a request for resource `name` find out what its root is. 
		 * We will just take the name to be everything up to the first dot (`.`) - to be made
		 * more flexible later.
		 * This makes a lot of decisions here much simpler.
		 * This would make PUT to create new resources very brittle unless the server
		 * can tell the client what the restrictions are.
		 * */
		def actorNameFor(resourceName: String): String = resourceName.takeWhile(_ != '.')

	}


}




