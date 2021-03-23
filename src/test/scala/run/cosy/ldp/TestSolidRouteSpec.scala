package run.cosy.ldp

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
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
	import akka.http.scaladsl.client.{RequestBuilding=>Req}

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
	
	def toUri(path: String): Uri = rootUri.withPath(Uri.Path(path))

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
		def newResource(baseDir: Uri, slug: Slug, text: String): Uri =
			Req.Post(baseDir, HttpEntity(text)).withHeaders(slug) ~>
				solid.route ~> check {
				status shouldEqual StatusCodes.Created
				val loc: Location = header[Location].get 
				loc.uri
			}
			
		def newContainer(baseDir: Uri, slug: Slug): Uri =
			Req.Post(baseDir).withHeaders(slug,BasicContainer.ldpcLinkHeaders) ~>
				solid.route ~> check {
					status shouldEqual StatusCodes.Created
					header[Location].get.uri
			}
		
		def read(url: Uri, text: String, times: Int = 1) =
			for (_ <- 1 to times)
				Req.Get(url).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.route ~> check {
					responseAs[String] shouldEqual text
				}
		
	end SolidTestPost
	
	"The Server" when {
		val rootC =  toUri("/")

		"started for the first time" in withServer { solid =>
			val test = new SolidTestPost(solid)
			
			Req.Get(rootUri).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.route ~> check {
				status shouldEqual StatusCodes.MovedPermanently
				header[Location].get shouldEqual Location(rootC)
			}
			
			//enable creation a new resource with POST and read it 3 times
			val newUri = test.newResource(rootC, Slug("Hello"), "Hello World!")
			newUri equals toUri("/Hello")
			test.read(newUri,"Hello World!", 3)

			//enable creation other resources with the same Slug and GET them too
			for (count <- (2 to 5).toList) {
				val createdUri = test.newResource(rootC, Slug("Hello"), s"Hello World $count!")
				assert(createdUri.path.endsWith(s"Hello_$count"))
				test.read(createdUri,s"Hello World $count!", 3)
			}
			
			Req.Delete(newUri) ~> solid.route ~> check {
				status shouldEqual StatusCodes.NoContent
			}
			
			Req.Get(newUri).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.route ~> check {
				status shouldEqual StatusCodes.Gone
			}

			Req.Get( toUri("/Hello.archive/")).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.route ~> check {
				status shouldEqual StatusCodes.NotFound
			}
			
			val blogDir = test.newContainer(rootC, Slug("blog"))
			blogDir shouldEqual toUri("/blog/")

			//enable creation a new resource with POST and read it 3 times
			val content =  "My First Blog Post is great"
			val firstBlogUri = test.newResource(blogDir, Slug("First Blog"),content)
			firstBlogUri equals toUri("/blog/FirstBlog")
			test.read(firstBlogUri,content, 3)

			for (count <- (2 to 5).toList) {
				val content =  s"My Cat had $count babies"
				val blogDir = test.newContainer(rootC, Slug("blog"))
				blogDir shouldEqual toUri(s"/blog_$count/")
				val firstBlogUri = test.newResource(blogDir, Slug(s"A Blog in $count"),content)
				firstBlogUri equals toUri(s"/blog_$count/ABlogIn$count")
				test.read(firstBlogUri, content, 2)
				//todo: read contents of dir to see contents added
			}
			
		}

		"restarted" in withServer { solid =>
			val test = new SolidTestPost(solid)

			//enable creation of more resources with the same Slug and GET them too
			for (count <- (6 to 9).toList) {
				val createdUri = test.newResource(rootC, Slug("Hello"), s"Hello World $count!")
				assert(createdUri.path.endsWith(s"Hello_$count"))
				test.read(createdUri,s"Hello World $count!", 3)
			}

			for (count <- (6 to 7).toList) {
				val blogDir = test.newContainer(rootC, Slug("blog"))
				blogDir shouldEqual toUri(s"/blog_$count/")
				//todo: read contents of dir to see contents added
			}
		}
	}

	override def afterAll(): Unit = deleteDir(dirPath)

}
