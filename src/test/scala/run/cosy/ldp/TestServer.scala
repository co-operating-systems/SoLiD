package run.cosy.ldp

import akka.http.scaladsl.model.Uri
import org.w3.banana.PointedGraph
import run.cosy.RDF.*
import run.cosy.RDF.Prefix.*
import run.cosy.RDF.ops.*

trait TestServer:
	lazy val podRdfURI: Rdf#URI = base.toRdf

	/** the root URi of the server */
	def base: Uri

	def path(u: String): Uri = base.withPath(Uri.Path(u))

	def podGr(pg: PointedGraph[Rdf]): Rdf#Graph = pg.graph.resolveAgainst(podRdfURI)

	/** the server as a simple map of graphs (we may want to extend this to content) */
	def db: Map[Uri, Rdf#Graph]

