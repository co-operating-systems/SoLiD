
package run.cosy.ldp

import run.cosy.RDF
import RDF.{given,_}
import RDF.ops.{given,_}
import akka.http.scaladsl.model.Uri
import cats.free.{Free,Cofree}
import cats.free.Free.liftF
import org.apache.jena.sparql.core.NamedGraph
import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.{StatusCode,StatusCodes}
import cats.Now
import cats.Applicative
import scala.util.Try
import scala.util.Success


/** 
 * Commands on the Server. All commands are executed agains on a resource
 * named by a URL.
 */ 
sealed trait LDPCmd[A]:
   def url: Uri

object LDPCmd {
	type LDPScript[A] = cats.free.Free[LDPCmd,A]

	// type NamedGraph = (Uri, Rdf#Graph)
	// type NamedGraphs = Map[Uri,Rdf#Graph]


	type Script[A] = Free[LDPCmd,A]

	/** 
	 * some Metadata on the resource from the server.
	 * @param the response on the message - whether the request failed or not
	 * @param url the URL of the resource
	 * @param headers: Seq of Http Headers (this could also be represented as a Graph) 
	 */
	case class Meta(url: Uri, code: StatusCode=StatusCodes.OK,  headers: Seq[HttpHeader]=Seq())
	
	/**
	 * A response to a request.
	 * The Metadata tells if the response succeeded, what the problems may have been, etc...
	 * The content, is an attempted parse of the stream to the object type.
	 * (todo?: generalise the type of the content to quite a lot more types, such
	 * as DataSets, Images, streams, ...) 
	 **/
	case class Response(meta: Meta, content: Try[Rdf#Graph])

	/** 
	 * Get response from URL. 
	 *  
 	 * ideas: one may want to pass 
	 *  - etag, date-since-previous-version, 
	 *  - authenticated agent info,
	 *  - content-type? 
	 *  - ...  
 	 * The return type is a T. To start we want this to be RDF types: Graph, DataSets. 
	 * How far could one generalise that? Allow T to be a stream too? A DOM? Clearly the more generality one allows
	 * the more the GET needs to specify restrictions on the return type.
	**/
	case class Get[A](url: Uri, k: Response => A) extends LDPCmd[A]

	/*
	 * CoFree as as defined in [[http://tpolecat.github.io/presentations/cofree/slides#19 tpolecat's Fixpoint slides]].
	 * 
	 * cats Cofree wraps the tail in an Eval, which may not be that useful for use cases here, Except
	 * perhaps that if the data structure gets somewhat large then duplicate may be quite expensive...
	 **/
	case class CoFree[F[_], A](head: A, tail: F[CoFree[F, A]])

	/*
	 * Graph Functor -- modelled on tpolecat's ProfF example
	 * Todo: look into what other functor would be more interesting for other: 
	 * a HashMap, a Graph? 
	 * todo: We use List here, as I can't figure out how to get CommutativeTraverse to Work for Set
	 **/
	case class GraF[A](default: Rdf#Graph, other: List[A]=List())
	
	/**
	 * A recursively defined DataSet where the first layer points to a Cofree graph with no
	 * metadata and the included sets contain metadata. 
	 * 
	 * This differs from RDF1.1 DataSets in that 
	 *  - DataSets can contain DataSets (which is [[https://lists.w3.org/Archives/Public/public-n3-dev/2021May/0012.html possible in N3]])
	 *  - the metadata is Http structured, whereas in RDF1.1 DS these are just URIs or bnodes
	 *  - this structure does not say anything about sharing of blank nodes
	 * 
	 */ 
	type RDataSet = GraF[Cofree[GraF,Meta]]
	
	/**
	 * MetaData on a Request for a DataSet
	 * eg Cofree(Meta(Uri("/.acl"),GraF(ttl"<> a TeaPot.", Set( ... other datasets ...))))
	 */ 
	type ReqDataSet = Cofree[GraF, Meta]
	import cats.{Applicative,CommutativeApplicative, Eval,Traverse}
	import cats.implicits._

	given GraFTraverse: cats.Traverse[GraF] with  
		override 
		def traverse[G[_]: Applicative, A, B](fa: GraF[A])(f: A => G[B]): G[GraF[B]] = 
			fa.other.traverse(f).map(ss => fa.copy(other=ss))

		override
		def foldLeft[A, B](fa: GraF[A], b: B)(f: (B, A) => B): B = fa.other.foldLeft(b)(f)
		override
		def foldRight[A, B](fa: GraF[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
	
	given CmdFunctor: cats.Functor[LDPCmd] with 
		def map[A, B](fa: LDPCmd[A])(f: A => B): LDPCmd[B] = fa match 
			case Get(url,k) => Get(url, k andThen f)

		

	def get(key: Uri): Script[Response] = liftF[LDPCmd, Response](Get[Response](key,identity))

	/**
	 * Build a Script to fetch owl:imports links starting from a graph at u, avoiding visted ones.
	 * See [[https://github.com/solid/authorization-panel/issues/210 Issue 210 on :imports]], though here
	 * we use owl.imports provisionally until a better name is chosen.
	 * 
	 * The script returns an ReqDataSet as defined here.
	 * It is quite possible that different branches could fetch the same graphs, but it should not lead to infinite loops.
	 * todo: there is parallelism here when fetching children: would it work to use Applicatives?
	 * todo: returning a Set may be better, but I had trouble getting Traverse to work on Set
	 */
	def fetchWithImports(u: Uri, visited: Set[Uri]=Set()) : Script[ReqDataSet] = for {
		gOpt <- get(u)
		ngs: ReqDataSet <- gOpt match 
			case Response(Meta(url, StatusCodes.OK, headers),Success(graph)) => 
				import cats.syntax.all.toTraverseOps
				val imports: List[Uri] = find(graph, ANY, owl.imports, ANY).toList.collect{ case Triple(_,_,o: Rdf#URI) => o.toAkka }
				val newVisited = visited+u
				val newImports = imports.filterNot(newVisited.contains(_))
				val covered = newVisited ++ newImports
				val lstOfScrpDs: List[Script[ReqDataSet]] = newImports.map(u => fetchWithImports(u, covered))
				lstOfScrpDs.sequence.map(subs => Cofree(Meta(u),Now(GraF(graph,subs.toList))))
			case Response(meta,_) => //any other result is problematic for the moment. 
			   //todo: pass more detailed error info into the result below - needed to explain problems
				cats.free.Free.pure[LDPCmd,ReqDataSet](Cofree(meta, Now(GraF(Graph.empty,Nil))))
	} yield ngs
	
}




