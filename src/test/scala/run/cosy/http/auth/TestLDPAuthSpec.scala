package run.cosy.http.auth

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.headers.{Accept, Authorization, GenericHttpCredentials, HttpChallenge, Location, `WWW-Authenticate`}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.AuthenticationFailedRejection
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsRejected
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.Timeout
import com.nimbusds.jose.jwk.JWK
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import run.cosy.{Solid, SolidTest}
import run.cosy.http.headers.Slug
import run.cosy.ldp.fs.BasicContainer
import org.w3.banana._

import java.nio.file.{Files, Path}
import concurrent.duration.DurationInt
import run.cosy.http.auth.WebServerAgent
import run.cosy.ldp.ResourceRegistry
import run.cosy.ldp.testUtils.TmpDir.{createDir, deleteDir}
import run.cosy.RDF.{given,*}
import run.cosy.RDF.ops.{given,*}
import run.cosy.http.RDFMediaTypes.*
import run.cosy.http.RdfParser.{rdfRequest,rdfUnmarshaller}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller


class TestSolidLDPAuthSpec extends AnyWordSpec with Matchers with ScalatestRouteTest {

	import akka.http.scaladsl.server.Directives
	import akka.http.scaladsl.client.{RequestBuilding=>Req}
	val security  = SecurityPrefix[Rdf]

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

	class SolidTestPost(solid: Solid, agent: Agent=new Anonymous):

		def newResource(baseDir: Uri, slug: Slug, ct: ContentType.WithFixedCharset, text: String): Uri =
			HttpRequest(HttpMethods.POST, baseDir, Seq[HttpHeader](slug), HttpEntity(ct,text)) ~>
				solid.routeLdp(agent) ~> check {
				status shouldEqual StatusCodes.Created
				val loc: Location = header[Location].get
				loc.uri
			}

		def newContainer(baseDir: Uri, slug: Slug): Uri =
			Req.Post(baseDir).withHeaders(slug,BasicContainer.ldpcLinkHeaders) ~>
				solid.routeLdp(agent) ~> check {
				status shouldEqual StatusCodes.Created
				header[Location].get.uri
			}

		def readRDF(url: Uri, expected: Rdf#Graph) =
			rdfRequest(url) ~> solid.routeLdp(agent) ~> check {
				given FromEntityUnmarshaller[Rdf#Graph] = rdfUnmarshaller(url)
				val result = responseAs[Rdf#Graph]
				assert(result.isIsomorphicWith(expected),s"result=$result ,\n expected=$expected")
				result
			}
	end SolidTestPost

	import org.tomitribe.auth.signatures.PEM
	val jwk = JWK.parseFromPEMEncodedObjects(TestHttpSigRSAFn.publicKeyPem)


	val lit: Rdf#Node = Literal(jwk.toJSONString,rdf.JSON)
	val keyGraph: Rdf#Graph = {URI("#key") -- security.publicKeyJwk ->- PointedGraph(lit, Graph.empty)}.graph

	"The Server" when {
		val rootC =  toUri("/")

		"started for the first time" in withServer { solid =>
			info(s"we GET <$rootUri>")
			Req.Get(rootUri).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.routeLdp(WebServerAgent) ~> check {
				status shouldEqual StatusCodes.MovedPermanently
				header[Location].get shouldEqual Location(rootC)
			}

			val test = new SolidTestPost(solid,WebServerAgent)

			info("create a new </key> with POST")
			val gtoSendTry: scala.util.Try[String] = turtleWriter.asString(keyGraph,"")
			assert(gtoSendTry.isSuccess)

			val newUri = test.newResource(rootC, Slug("key"), `text/turtle`.toContentType, gtoSendTry.get)
			newUri equals toUri("/key")

			info("verify it can be read and that it is isomorphic to what was sent")

			val canonicalKeyGraph = keyGraph.resolveAgainst(URI(newUri.toString()))
			test.readRDF(newUri,canonicalKeyGraph)

			info("GET the same document by authenticating with the key")

//			info("create 3 more resources with the same Slug and GET them too")
//			for (count <- (2 to 5).toList) {
//				val createdUri = test.newResource(rootC, Slug("Hello"), s"Hello World $count!")
//				assert(createdUri.path.endsWith(s"Hello_$count"))
//				test.read(createdUri,s"Hello World $count!", 3)
//			}
//
//			info("Delete the first created resource </Hello>")
//			Req.Delete(newUri) ~> solid.routeLdp(WebServerAgent) ~> check {
//				status shouldEqual StatusCodes.NoContent
//			}
//
//			info("Try GET deleted </Hello>")
//			Req.Get(newUri).withHeaders(Accept(MediaRanges.`*/*`)) ~> solid.routeLdp(WebServerAgent) ~> check {
//				status shouldEqual StatusCodes.Gone
//			}
//
//			info("Try GET its archive </Hello.archive/>")
//			Req.Get( toUri("/Hello.archive/")).withHeaders(Accept(MediaRanges.`*/*`))
//				~> solid.routeLdp(WebServerAgent) ~> check {
//				status shouldEqual StatusCodes.NotFound
//			}
//
//			info("Create a new Container </blog/>")
//			val blogDir = test.newContainer(rootC, Slug("blog"))
//			blogDir shouldEqual toUri("/blog/")
//
//			info("enable creation a new resource with POST in the new container and read it 3 times")
//			val content =  "My First Blog Post is great"
//			val firstBlogUri = test.newResource(blogDir, Slug("First Blog"),content)
//			firstBlogUri equals toUri("/blog/FirstBlog")
//			test.read(firstBlogUri,content, 3)
//
//			for (count <- (2 to 5).toList) {
//				val content =  s"My Cat had $count babies"
//				val blogDir = test.newContainer(rootC, Slug("blog"))
//				blogDir shouldEqual toUri(s"/blog_$count/")
//				val firstBlogUri = test.newResource(blogDir, Slug(s"A Blog in $count"),content)
//				firstBlogUri equals toUri(s"/blog_$count/ABlogIn$count")
//				test.read(firstBlogUri, content, 2)
//				//todo: read contents of dir to see contents added
			}
		}

//		"restarted" in withServer { solid =>
//			val test = new SolidTestPost(solid, WebServerAgent)
//
//			info("create more resources with the same Slug(Hello) and GET them too - the deleted one is not overwritten")
//			for (count <- (6 to 9).toList) {
//				val createdUri = test.newResource(rootC, Slug("Hello"), s"Hello World $count!")
//				assert(createdUri.path.endsWith(s"Hello_$count"))
//				test.read(createdUri,s"Hello World $count!", 3)
//			}
//
//			info("create more blogs and GET them too with same slug. Numbering continues where it left off.")
//			for (count <- (6 to 7).toList) {
//				val blogDir = test.newContainer(rootC, Slug("blog"))
//				blogDir shouldEqual toUri(s"/blog_$count/")
//				//todo: read contents of dir to see contents added
//			}
//		}


	override def afterAll(): Unit = deleteDir(dirPath)

}



