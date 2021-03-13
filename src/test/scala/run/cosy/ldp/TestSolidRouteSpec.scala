package run.cosy.ldp

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.http.scaladsl.client.RequestBuilding.{Get, Post}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaRanges, StatusCodes, Uri}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.Timeout
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import run.cosy.Solid
import run.cosy.http.Headers.Slug
import run.cosy.ldp.fs.BasicContainer

import java.nio.file.{Files, Path}
import concurrent.duration.DurationInt

class TestSolidRouteSpec extends AnyWordSpec with Matchers with ScalatestRouteTest {

	import akka.http.scaladsl.server.Directives

	def tmpDir(testCode: Path => Any): Unit =
		import java.nio.file.Path
		val dir: Path = Files.createTempDirectory("cosyTest")
		try {
			testCode(dir) // "loan" the fixture to the test
		} finally
//			 Always gets called, even if test failed.
			import java.nio.file.{Path,FileVisitOption, FileVisitor}
			import java.util.Comparator
			import scala.jdk.StreamConverters.{given,*}
			val files: LazyList[Path] = Files.walk(dir).sorted(Comparator.reverseOrder()).toScala(LazyList)
			files.map(_.toFile).foreach(_.delete)
	end tmpDir
	
	// This test does not use the classic APIs,
	// so it needs to adapt the system:
	import akka.actor.typed.scaladsl.adapter.{given,*}
	given typedSystem: ActorSystem[Nothing] = system.toTyped
	given registry: ResourceRegistry = ResourceRegistry(typedSystem)
	implicit val timeout: Timeout = Timeout(5000.milliseconds)
	implicit val scheduler: Scheduler = typedSystem.scheduler
	
	val rootUri: Uri = Uri("http://localhost:8080")
	val testKit = ActorTestKit()
	import akka.http.scaladsl.model.headers.*
	
	def toUri(path: String) = rootUri.withPath(Uri.Path(path))
	
	"Server" in tmpDir { dirPath =>
		val rootCntr: Behavior[BasicContainer.Cmd] = BasicContainer(rootUri,dirPath)
		val rootActr: ActorRef[BasicContainer.Cmd] = testKit.spawn(rootCntr,"solid")
	//	val probe = TestProbe[HttpResponse]()

		val solid = new Solid(rootUri,dirPath,registry,rootActr)
		
		def post(slug: String, text: String, count: String ="") = 	
			Post(toUri("/"),HttpEntity(text)).withHeaders(Slug(slug)) ~>
				solid.route ~> check {
					status shouldEqual StatusCodes.Created
					header[Location].get shouldEqual Location(Uri(s"http://localhost:8080/Hello$count"))
				}
		
		def read(text: String, count: String, times: Int = 1) = 
			for (_ <- 1 to times)
				Get(toUri("/Hello"+count)).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.route ~> check {
					 responseAs[String] shouldEqual text
				}
			

		// 1. Post and chack that multiple gets function.
		post("Hello", "Hello World!")
		read("Hello World!","",3)
		
		for (count <- (2 to 5).toList.map("_"+_.toString)) { 
			post("Hello",s"Hello World $count!",count)
			read(s"Hello World $count!",count,3)
		}

	}

	override def afterAll(): Unit = testKit.shutdownTestKit()

}
