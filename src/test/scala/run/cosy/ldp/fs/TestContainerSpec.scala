package run.cosy.ldp.fs

import akka.actor.testkit.typed.CapturedLogEvent
import akka.actor.testkit.typed.Effect._
import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, TestInbox}
import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, MediaRange, MediaRanges, MediaTypes}
import run.cosy.ldp.ResourceRegistry
import run.cosy.ldp.fs.BasicContainer

import java.nio.file.Files
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.slf4j.event.Level
import run.cosy.http.Headers.Slug
import akka.testkit.TestKit
import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.{Created, OK}
import akka.http.scaladsl.model.headers.{Accept, Location}
import akka.stream.{ActorMaterializer, Materializer}
import cats.data.NonEmptyList

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class TestContainerSpec extends AnyFlatSpec with BeforeAndAfterAll with Matchers {
	import akka.http.scaladsl.model.Uri

	import java.nio.file
	import java.nio.file.{Files, Path}

	def tmpDir(testCode: Path => Any): Unit = 
		import java.nio.file.Path
		val dir: Path = Files.createTempDirectory("cosyTest")
		try {
			testCode(dir) // "loan" the fixture to the test
		} finally
			// Always gets called, even if test failed.
			import java.nio.file.{Path,FileVisitOption, FileVisitor}
			import java.util.Comparator
			import scala.jdk.StreamConverters.{given,*}
			val files: LazyList[Path] = Files.walk(dir).sorted(Comparator.reverseOrder()).toScala(LazyList)
			files.map(_.toFile).foreach(_.delete)
	end tmpDir

	val testKit = ActorTestKit()

	"Root Container" should "be started" in tmpDir { dirPath =>
		
		import akka.http.scaladsl.model.HttpResponse
		val rootUri = Uri("http://localhost:8080")

		given registry: ResourceRegistry = ResourceRegistry(testKit.system)
		val rootCntr: Behavior[BasicContainer.Cmd] = BasicContainer(rootUri,dirPath)
		val rootActr: ActorRef[BasicContainer.Cmd] = testKit.spawn(rootCntr,"solid")
		val probe = testKit.createTestProbe[HttpResponse]()
		given classic : ActorSystem = testKit.system.classicSystem
		given ec: ExecutionContext = testKit.system.executionContext

		{
			//create Hello
			import _root_.run.cosy.http.Encoding.{given, *}
			rootActr ! BasicContainer.Do(
				HttpRequest(
					POST,
					rootUri.withPath(Uri.Path.SingleSlash),
					Seq(Slug("Hello".asClean)),
					HttpEntity(ContentTypes.`text/plain(UTF-8)`, "Hello World")
				),
				probe.ref
			)
			val HttpResponse(status, hdrs, response, protocol) = probe.receiveMessage()
			assert(status == Created)
			assert(hdrs.contains(Location(rootUri.withPath(Uri.Path.Empty / ("Hello")))))
			assert(response.contentType.mediaType == MediaTypes.`text/plain`)
		}
		{
			// Read Hello
			rootActr ! BasicContainer.Route(
				NonEmptyList("Hello", Nil),
				HttpRequest(
					GET,
					rootUri.withPath(Uri.Path / "Hello"),
					Seq(Accept(MediaRanges.`*/*`))
				), probe.ref
			)
			val HttpResponse(status, hdrs, response, protocol) = probe.receiveMessage()
			assert(status == OK)
		//	assert(hdrs.contains(Location(rootUri.withPath(Uri.Path.Empty / ("Hello.txt")))))
			assert(response.contentType.mediaType == MediaTypes.`text/plain`)
			val f: Future[String] = response.httpEntity.dataBytes.runFold("")((s, bs) => s+bs.decodeString("UTF-8"))
			f.onComplete{
				case Success(str) => assert(str == "Hello World")
				case Failure(e)	=> assert(false)
			}
		}
		
	}

	override def afterAll(): Unit = testKit.shutdownTestKit()
}
