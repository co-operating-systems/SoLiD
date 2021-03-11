package run.cosy.ldp.fs

class TestContainerFn extends munit.FunSuite {
	import akka.http.javadsl.model.MediaType
	import akka.http.scaladsl.model
	import model.ContentTypes._
	import model.HttpMethods.POST
	import model.headers.`Content-Type`
	import model.{ContentType, ContentTypes, HttpRequest, Uri, HttpEntity}
	import run.cosy.http.Headers.Slug
	import run.cosy.http.{Encoding => cEncoding}
	import run.cosy.ldp.fs.BasicContainer.createName

	import java.time.{Clock, Instant, ZoneId}
	
	val testClock = Clock.fixed(Instant.EPOCH,ZoneId.of("UTC"))
	
	def testSlug(slugTxt: String, ct: ContentType, expectedLinkName: String, expectedLinkTo: String): Unit =
		import akka.util.ByteString
		import cEncoding.*
		val req = HttpRequest(POST, Uri("/"),
			         Option(slugTxt).map(t=>Slug(t.asClean)).toSeq,
						HttpEntity(Option(ct).getOrElse(`application/octet-stream`),ByteString())
					)                                                  
		val (name, linkTo) = createName(req,testClock)
		assertEquals(name,expectedLinkName,req)
		assertEquals(linkTo,expectedLinkTo,req)
	end testSlug
	
	test("Test Slug") {
		import akka.http.scaladsl.model.{MediaTypes=>mt}
		import akka.http.scaladsl.model.HttpCharsets.*
	
		//todo: clearly the default encoding from Akka, will not always be to everyone's liking
		testSlug("Readme.txt",ContentTypes.`text/plain(UTF-8)`,"Readme","Readme.conf") 
		testSlug("MyCatFelix",mt.`image/jpeg`.toContentType,"MyCatFelix","MyCatFelix.jpe")
		testSlug("MyCatFelix.jpg",mt.`image/gif`.toContentType,"MyCatFelix","MyCatFelix.gif")
		testSlug("Felix had a baby.md",mt.`text/markdown`.withCharset(`UTF-8`),"Felixhadababy","Felixhadababy.markdown")
		testSlug(null, mt.`video/mp4`.toContentType,"19700101-0","19700101-0.mp4")
		testSlug(null, null,"19700101-0","19700101-0.a")
		testSlug(null, mt.`application/octet-stream`.toContentType,"19700101-0","19700101-0.a")
	}

}
