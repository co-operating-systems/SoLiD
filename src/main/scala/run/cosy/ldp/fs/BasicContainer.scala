package run.cosy.ldp.fs

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, PreRestart}
import akka.http.scaladsl.model
import akka.http.scaladsl.model.StatusCodes.{Created, Gone, InternalServerError, MovedPermanently, NotFound, NotImplemented, OK, PermanentRedirect}
import akka.http.scaladsl.model.headers.{HttpChallenge, Link, LinkParam, LinkValue, `Content-Type`, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.server.ContentNegotiator.Alternative
import akka.http.scaladsl.server.ContentNegotiator.Alternative.ContentType
import akka.http.scaladsl.server.{RequestContext, RouteResult}
import akka.stream.scaladsl.{Concat, FileIO, Merge, RunnableGraph, Source}
import akka.stream.{ActorMaterializer, IOResult, Materializer}
import akka.util.ByteString
import akka.{Done, NotUsed}
import run.cosy.http.headers.Slug
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
import akka.event.slf4j.Slf4jLogger
import run.cosy.http.RDFMediaTypes
import run.cosy.http.auth.{Agent, WebKeyidAgent, WebServerAgent}
import run.cosy.ldp.fs.BasicContainer
import run.cosy.ldp.fs.BasicContainer.{Cmd, Route, WannaDo}
import run.cosy.ldp.ResourceRegistry
import scalaz.NonEmptyList.nel
import scalaz.{ICons, INil, NonEmptyList}

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

	import akka.http.scaladsl.model.HttpHeader
	import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok

	import java.nio.file.{FileTreeIterator, FileVisitOption}
	import java.time.{Clock, Instant}
	import java.util.{Spliterator, Spliterators}
	import java.util.stream.{Stream, StreamSupport}

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
	
	given clock: Clock = Clock.systemDefaultZone

	def santiseSlug(slugTxt: String): String =
		val santized = slugTxt.takeWhile(_ != '.').filterNot(c => Remove.contains(c) || c.isWhitespace)
		santized.substring(0, Math.min(MaxFileName, santized.size))
	
	val timeFormat = DateTimeFormatter.ofPattern("yyyyMMdd-N").withZone(ZoneId.of("UTC"))
	
	def createTimeStampFileName(using clock: Clock): String =  timeFormat.format(LocalDateTime.now(clock))

	//todo: add Content-Encoding, Content-Language
	def linkToName(linkName: String, cts: ContentType) =
		linkName +"."+cts.contentType.mediaType.fileExtensions.headOption.getOrElse("bin")

	/** @return (linkName, linkTo) strings */
	def createLinkNames(req: HttpRequest)(using clock: Clock): (String, String) = {
		val linkName = createNewResourceName(req)
		val linkTo = linkToName(linkName, req.entity.contentType)
		(linkName, linkTo)
	}

	def createNewResourceName(req: HttpRequest)(using clock: Clock): String = {
		req.headers.collectFirst{case Slug(name) => name}
			.map(slug => santiseSlug(slug.toString))
			.getOrElse(createTimeStampFileName)
	}

//	import java.nio.file.{FileTreeWalker,FileVisitOption}
//	def ls(start: Path, options: FileVisitOption*): Source[FileTreeWalker.Event, NotUsed] =
//		val iterator = new FileTreeIterator(start, 1, options)
//		val factory = () => try {
//			val spliterator = Spliterators.spliteratorUnknownSize(iterator, Spliterator.DISTINCT)
//			StreamSupport.stream(spliterator, false).onClose(iterator.close)
//		} catch {
//			case e@(_: (Error | RuntimeException)) =>
//				iterator.close()
//				throw e
//		}
//		import akka.stream.scaladsl.StreamConverters
//		//todo: factory throws an Exception, where is that caught?
//		StreamConverters.fromJavaStream(factory)
//	end ls

	val ldpc = Uri("http://www.w3.org/ns/ldp#BasicContainer")
	val ldpr = Uri("http://www.w3.org/ns/ldp#Resource")
	//get all the  URIs with link rel="type"
	def filterLDPTypeLinks(links: Seq[Link]): Seq[Uri] =
		import akka.http.scaladsl.model.headers.LinkParams.rel
		links.flatMap{ link =>
			link.values.collect {
				case LinkValue(uri, params) if params.collectFirst({case rel("type") => true}).isDefined => uri
			}
		}

	val Ok(ldpcLinkHeaders,_) = HttpHeader.parse(
		"Link",
		"""<http://www.w3.org/ns/ldp#BasicContainer>; rel="type",
		  |<http://www.w3.org/ns/ldp#Resource>; rel="type"""".stripMargin.replace("\n", "")
	)

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
		context.log.info(s"started actor for <${containerUrl}> in dir <file:/${dir.toAbsolutePath}>")
		new BasicContainer(containerUrl, dir, new Spawner(context)).behavior
	}

	sealed trait Cmd

	//trait for a Cmd that is to be acted upon
	sealed trait Act extends Cmd
	sealed trait Route extends Cmd
	sealed trait Info extends Cmd

	sealed trait ReqMsg {
		val req: HttpRequest
		val from: Agent
		val replyTo: ActorRef[HttpResponse]
	}

	/**
	 * @param path the path to the final resource */
	final case class RouteMsg(
		path: NonEmptyList[String],
		from: Agent,
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqMsg with Route {
		def name: String = path.head

		// check that path is not empty before calling  (anti-pattern)
		def next: ReqMsg with Route = path match
			case NonEmptyList(_, INil()) => WannaDo(from, req, replyTo)
			case NonEmptyList(_, ICons(h,tail)) => RouteMsg(nel(h,tail), from, req, replyTo)
	}

	/** a command to be executed on the resource on which it arrives, after being authorized */
	final case class Do(
		from: Agent,
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqMsg with Act

	/** message has arrived, but still needs to be authorized */
	final case class WannaDo(
		 from: Agent,
		 req: HttpRequest,
		 replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqMsg with Route

	/** Message for initial creation of container, after creation of dir. HttpRequest in Do.  */
	case class CreateContainer(
		name: Path, cmd: Do
	)(using
	  val mat: Materializer, val ec: ExecutionContext
	) extends Act

	// responses from child to parent
	case class ChildTerminated(name: String) extends Info

}

/**
 * Class of Resource objects, mapped to a path on the file system.
 * Our convetion for files on the FS at present (later to be modularized) is designed
 * to avoid needing to load the container contents into memory, and to allow content negotiation
 * to work both locally and on the file system. These are the rules:
 *  1. The root of the file is the string up to the first "." (same with directories)
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
 *  1. If a resource `cat` is deleted the actor will create a `cat.archive` directory and move all associatd files there.
 *     (this is to be able to recover from a problem half way through deleting.)
 *     The `cat` link is removed and a new one created to the archive directory. This will allow us to enforce a memory
 *     that a file is no longer available. Better methods of doing this could be possible.
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

	import BasicContainer.{Do, ReqMsg, RouteMsg, linkToName, santiseSlug}
	import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
	import akka.http.scaladsl.model.MediaTypes.{`text/plain`, `text/x-java-source`}
	import akka.http.scaladsl.model.headers.{Link, LinkParams, LinkValue, Location, `Content-Type`}
	import akka.http.scaladsl.model.{ContentTypes, HttpCharsets, HttpEntity, HttpMethods}
	import HttpMethods.{GET, POST, DELETE}
	import Resource.CreateResource
	import run.cosy.ldp.fs.{Ref,CRef,RRef}
	import run.cosy.ldp.fs.Attributes.{Attributes, Archived, OtherAtt, ActorFileAttr, Other}

	import java.nio.file.attribute.BasicFileAttributes
	import java.time.Instant
	//Cashed Contents of Directory
	type Contents = HashMap[String,Ref|Other]
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

	/** Return a Source for reading the relevant files for this directory.
	 * Note: all symbolic links and dirs are our resources, so long as they
	 * don't have a `.` in them.
	 *
	 * todo: now that we have symlinks to archives, we would need to test every symlink for
	 * what it links to! So we should perhaps instead use a plain file for deleted resources!
	 * */
	val dirList: Source[(Path, BasicFileAttributes), NotUsed] = Source.fromGraph(
		DirectoryList(dirPath,1){ (path: Path, att: BasicFileAttributes) =>
			att.isSymbolicLink || (att.isDirectory && !path.getFileName.toString.contains('.'))
		})
	val prefix: Source[String,NotUsed] = Source(
		List("@prefix stat: <http://www.w3.org/ns/posix/stat#> .\n",
		"@prefix ldp: <http://www.w3.org/ns/ldp#> .\n\n"))

	def containsAsTurtle(path: Path, att: BasicFileAttributes): String = {
		val filename = path.getFileName.toString + { if att.isDirectory then "/" else "" }
		s"""<> ldp:contains <$filename> .
			|    <$filename> stat:size ${att.size};
			|        stat:mtime ${att.lastModifiedTime().toMillis};
			|        stat:ctime ${att.creationTime().toMillis} .
			|""".stripMargin
	}

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


		import akka.stream.alpakka.file.scaladsl.Directory

		import java.nio.file.attribute.BasicFileAttributes
		import java.time.Instant
		import scala.annotation.tailrec

		def Authorize(wd: WannaDo): Behavior[Cmd] =
			context.log.info(s"in Authorize for $dirPath. received $wd")
			if List(WebServerAgent,WebKeyidAgent(Uri("/user/key#"))).exists(_ == wd.from) then
				import wd.{given,*}
				context.context.self ! Do(from,req,replyTo)
			else wd.replyTo ! HttpResponse(StatusCodes.Unauthorized,
				Seq(`WWW-Authenticate`(HttpChallenge("Signature",s"$dirPath")))
			)
			Behaviors.same


		lazy val start: Behavior[Cmd] = {
			Behaviors.receiveMessage[Cmd] { (msg: Cmd) =>
				import BasicContainer.{ChildTerminated, CreateContainer}
				msg match
					case wd: WannaDo => Authorize(wd)
					case act: Do => run(act)
					case create: CreateContainer =>
						// we don't do much at this point. For later
						create.cmd.replyTo ! HttpResponse(
							Created,
							Seq(Location(containerUrl.withPath(containerUrl.path/"")),
								BasicContainer.ldpcLinkHeaders
							)
						)
						Behaviors.same
					case routeMsg: RouteMsg => routeHttpReq(routeMsg)
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
		def getRef(name: String): Option[(Ref|Other, Dir)] =
			context.log.info(s"in <$dirPath>.getRef($name)")
			contains.get(name).map { (v : Ref|Other)  =>
				v match
				case ref: Ref => (ref, Dir.this)
				case fa: ActorFileAttr  =>
					val r = context.spawn(fa,urlFor(name))
					(r, new Dir(contains + (name -> r), counters))
				case ar: Other => (ar, Dir.this)
			}.orElse {
				import java.io.IOException
				import java.nio.file.LinkOption.NOFOLLOW_LINKS
				import java.nio.file.attribute.BasicFileAttributes
				val path = dirPath.resolve(name)  //todo: also can throw exception
				context.log.info(s"resolved <$dirPath>.resolve($name)=$path")
				Attributes.forPath(path) match
					case Success(att: ActorFileAttr) =>
						val r: Ref = context.spawn(att,urlFor(name))
						Some((r, new Dir(contains + (name -> r), counters)))
					case Success(aro : Other) =>
						Some((aro, new Dir(contains + (name -> aro), counters)))
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
			}
		end getRef


		// state monad	fn. create symlink to new file (we don't have an actor for it yet)
		def createSymLink(linkName: String, linkTo: String): Try[(RRef, Dir)] =
			Attributes.createLink(dirPath, linkName,linkTo).map{ att =>
				val ref = context.spawnSymLink(att,urlFor(linkName))
				(ref, new Dir(contains + (linkName -> ref),counters))
			}

		def createDir(newDirName: String): Try[(CRef, Dir)] =
			Attributes.createDir(dirPath, newDirName) .map{ att =>
				val ref = context.spawnDir(att,urlFor(newDirName))
				(ref, new Dir(contains + (newDirName -> ref),counters))
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
						HttpEntity(RDFMediaTypes.`text/turtle`.toContentType,
							Source.combine(prefix,dirList.map(containsAsTurtle))(Concat(_)).map(s =>ByteString(s)))
					)
					Behaviors.same
				case POST =>  //create resource
					//todo: create sub container for LDPC Post
					import java.time.Clock
					import BasicContainer.{createLinkNames, filterLDPTypeLinks,createNewResourceName,ldpc,ldpr}
					import BasicContainer.{given Clock}
					val types = filterLDPTypeLinks(headers[Link])
					if types.contains(ldpc) then
						//todo: should one also parse the body first to check that it is well formed? Or should that
						//   be left to the created actor - which may have to delete itself if not.
						val newDirName = createNewResourceName(msg.req)
						val response: Try[(CRef, Dir)] = createDir(newDirName).recoverWith {
							case e : FileAlreadyExistsException =>   // this is the pattern of a state monad!
								val (nextId, newDir) = nextCounterFor(newDirName)
								val nextDirName = newDirName+"_"+nextId
								newDir.createDir(nextDirName)
						}
						response match {
							case Success((cref, dir)) =>
								cref.actor ! BasicContainer.CreateContainer(cref.att.path,msg)
								dir.start
							case Failure(e) =>
								HttpResponse(InternalServerError, Seq(),
									HttpEntity(`text/x-java-source`.withCharset(HttpCharsets.`UTF-8`), e.toString) )
								Behaviors.same
						}
					else //create resource
						val (linkName: String, linkTo: String) = createLinkNames(msg.req)
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
					end if
				case DELETE =>
					//todo: check that there are no contents in the directory then delete
					HttpResponse(NotImplemented, Seq(), entity = s"have not implemented  ${msg.req.method} for ${msg.req.uri}")
					Behaviors.same
				//todo: create new PUT request and forward to new actor?
				//or just save the content to the file?
				case _ =>
					HttpResponse(NotImplemented, Seq(), entity = s"have not implemented  ${msg.req.method} for ${msg.req.uri}")
					Behaviors.same
		end run

//		def listContents: Source[ByteString, NotUsed] = {
//			import java.nio.file.FileTreeWalker
//			BasicContainer.ls(dirPath).map{ (e: FileTreeWalker.Event) =>
//				ByteString(
//					s"""<> ldp:contains ${e.}
//						|""".stripMargin)
//			}
//		}

		protected
		def routeHttpReq(msg: RouteMsg): Behavior[Cmd] = {
			context.log.info(
				s"received ${msg.req.method.value} <${msg.req.uri}> in ${dirPath.toFile.getAbsolutePath} in container with uri $containerUrl")

			msg.next match
				case doit: WannaDo =>
					if doit.req.uri.path.endsWithSlash then
						forwardToContainer(msg.name, msg)
					else forwardMsgToResourceActor(msg.name, doit)
				case route: RouteMsg => forwardToContainer(msg.name, route)
		}

		//todo: do we need a name cleanup happen? Perhaps we don't need that for containers?
		//  but getRef can also return a RRef... So `cat.jpg`
		//  here would return a `cat.jpg` RRef rather than `cat` or a `CRef`
		//  this indicates that the path name must be set after checking the attributes!
		def forwardToContainer(name: String, msg: ReqMsg with Route): Behavior[Cmd] = {
			if name.contains('.') then
				msg.replyTo ! HttpResponse(NotFound,
					entity=HttpEntity("This Solid server serves no resources with a '.' char in path segments (except for the last `file` segment)."))
				Behaviors.same
			else getRef(name) match
				case Some(x, dir) =>
					x match
					case CRef(att, actor) => actor ! msg
					case RRef(att, actor) => // there is no container, so redirect to resource
						msg match
						case WannaDo(agent, req, replyTo) =>  //we're at the path end, so we can redirect
							val uri = msg.req.uri
							val redirectTo = uri.withPath(uri.path.reverse.tail.reverse)
							replyTo ! HttpResponse(MovedPermanently, Seq(Location(redirectTo)))
						case RouteMsg(path, agent, req, replyTo) => // the path passes through a file, so it must end here
							replyTo ! HttpResponse(NotFound, Seq(), s"Resource with URI ${req.uri} does not exist")
					case _: Archived => msg.replyTo ! HttpResponse(Gone)
					case _: OtherAtt => msg.replyTo ! HttpResponse(NotFound)
					dir.start
				case None =>
					msg.replyTo ! HttpResponse(NotFound,
											entity = HttpEntity(s"""Resource with URI ${msg.req.uri} does not exist."""))
					Behaviors.same
		}
		end forwardToContainer
		
		
		/**
		 * Forward message to child resource (not container)
		 * @param name Content-Negotiation root name: e.g. `cat` can return `cat.jpg` or `cat.png`
		 * @param msg the HttpRequest
		 * @return A new Behavior, updated if a new child actor is created.
		 */
		protected
		def forwardMsgToResourceActor(name: String, msg: WannaDo): Behavior[Cmd] =
			val dotLessName = actorNameFor(name)
			getRef(dotLessName) match
			case Some(x,dir) =>
				x match
				case CRef(_,actor) =>	msg.replyTo ! {
					if (dotLessName == name)
						val uri = msg.req.uri
						HttpResponse(MovedPermanently, Seq(Location(uri.withPath(uri.path / ""))))
					else HttpResponse(NotFound)
				}
				case RRef(_, actor) => actor ! msg
				case _: Archived => msg.replyTo ! HttpResponse(Gone)
				case _: OtherAtt => msg.replyTo ! HttpResponse(NotFound)
				dir.start
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




