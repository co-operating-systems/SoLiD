package run.cosy.ldp

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.http.scaladsl.client.RequestBuilding.{Get, Post}
import akka.http.scaladsl.model.headers.{Accept, Location}
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaRanges, StatusCodes, Uri}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.Timeout
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import run.cosy.{Solid, SolitTest}
import run.cosy.http.Headers.Slug
import run.cosy.ldp.fs.BasicContainer

import java.nio.file.{Files, Path}
import concurrent.duration.DurationInt
import run.cosy.ldp.testUtils.TmpDir.{createDir, deleteDir}

class TestSolidRouteSpec extends AnyWordSpec with Matchers with ScalatestRouteTest {

	import akka.http.scaladsl.server.Directives

	val dirPath: Path = createDir("solidTest_")
	
	// This test does not use the classic APIs,
	// so it needs to adapt the system:
	import akka.actor.typed.scaladsl.adapter.{given,*}
	given typedSystem: ActorSystem[Nothing] = system.toTyped
	given registry: ResourceRegistry = ResourceRegistry(typedSystem)
	implicit val timeout: Timeout = Timeout(5000.milliseconds)
	implicit val scheduler: Scheduler = typedSystem.scheduler
	
	val rootUri: Uri = Uri("http://localhost:8080")

	import akka.http.scaladsl.model.headers
	
	def toUri(path: String) = rootUri.withPath(Uri.Path(path))

	def withServer(test: Solid => Any): Unit =
		val testKit = ActorTestKit()
		val rootCntr: Behavior[BasicContainer.Cmd] = BasicContainer(rootUri, dirPath)
		val rootActr: ActorRef[BasicContainer.Cmd] = testKit.spawn(rootCntr, "solid")
		val solid = new Solid(rootUri, dirPath, registry, rootActr)
		try {
			test(solid)
		} finally testKit.shutdownTestKit()
	end withServer
			
	class SolidTestPost(solid: Solid):
		def post(slug: String, text: String, count: String = "") =
			Post(toUri("/"), HttpEntity(text)).withHeaders(Slug(slug)) ~>
				solid.route ~> check {
				status shouldEqual StatusCodes.Created
				header[Location].get shouldEqual Location(Uri(s"http://localhost:8080/Hello$count"))
			}
		def read(text: String, count: String, times: Int = 1) =
			for (_ <- 1 to times)
				Get(toUri("/Hello" + count)).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.route ~> check {
					responseAs[String] shouldEqual text
				}
	end SolidTestPost
	
	"The Server" when {
		"started for the first time" in withServer { solid =>
			val test = new SolidTestPost(solid)

			//enable creation a new resource with POST and read it 3 times
			test.post("Hello", "Hello World!")
			test.read("Hello World!", "", 3)

			//enable creation other resources with the same Slug and GET them too
			for (count <- (2 to 5).toList.map("_" + _.toString)) {
				test.post("Hello", s"Hello World $count!", count)
				test.read(s"Hello World $count!", count, 3)
			}
		}

		"restarted" in withServer { solid =>
			val test = new SolidTestPost(solid)

			//enable creation more resources with the same Slug and GET them too
			for (count <- (6 to 9).toList.map("_" + _.toString)) {
				test.post("Hello", s"Hello World $count!", count)
				test.read(s"Hello World $count!", count, 3)
			}
		}
	}

	override def afterAll(): Unit = 
		deleteDir(dirPath)

}
