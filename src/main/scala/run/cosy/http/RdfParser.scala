package run.cosy.http

import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
import akka.http.scaladsl.model.{ContentType, HttpHeader, HttpRequest, HttpResponse, StatusCode, Uri}

import scala.concurrent.{ExecutionContext, Future}
import org.w3.banana._
import org.w3.banana.syntax._
import org.w3.banana.jena.Jena
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import org.apache.jena.graph.Graph
import org.w3.banana.io.RDFReader

import scala.util.{Failure, Try}
import scala.util.control.NoStackTrace

object RdfParser {
	import run.cosy.RDF.{given,*}
	import ops.{given,*}
	import RDFMediaTypes.*

	/** @param base: the URI at which the document was resolved */
	def rdfUnmarshaller(base: Uri): FromEntityUnmarshaller[Rdf#Graph] =
		//todo: this loads everything into a string - bad
		PredefinedFromEntityUnmarshallers.stringUnmarshaller flatMapWithInput { (entity, string) ⇒
			def parseWith[T](reader: RDFReader[Rdf,Try,T]) = Future.fromTry {
				reader.read(new java.io.StringReader(string), base.toString)
			}
			//todo: use non blocking parsers
			entity.contentType.mediaType match
				case `text/turtle` => parseWith(turtleReader)
				case `application/rdf+xml` => parseWith(rdfXMLReader)
				case `application/n-triples` => parseWith(ntriplesReader)
				case `application/ld+json` => parseWith(jsonldReader)
				// case `text/html` => new SesameRDFaReader()
				case _ => FastFuture.failed(MissingParserException(string.take(400)))
		}

	def rdfRequest(uri: Uri): HttpRequest =
		import akka.http.scaladsl.model.headers.Accept
		HttpRequest(uri=uri.withoutFragment)
			.addHeader(Accept(`text/turtle`,`application/rdf+xml`,
				`application/n-triples`,
				`application/ld+json`.withQValue(0.8)//todo: need to update parser in banana
				//`text/html`.withQValue(0.2)
		)) //we can't specify that we want RDFa in our markup

	def unmarshalToRDF(
		resp: HttpResponse, base: Uri
	)(using ExecutionContext, Materializer): Future[IResponse[Rdf#Graph]] =
		import resp._
		given FromEntityUnmarshaller[Rdf#Graph] = RdfParser.rdfUnmarshaller(base)
		import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
		Unmarshal(entity).to[Rdf#Graph].map { g =>
			IResponse[Rdf#Graph](base, status, headers, entity.contentType, g)
		}


}

