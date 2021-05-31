package run.cosy.ldp

import run.cosy.RDF
import RDF.{given,_}
import RDF.ops.{given,_}
import akka.http.scaladsl.model.Uri
import cats.free.{Free,Cofree}
import cats.free.Free.liftF
import org.apache.jena.sparql.core.NamedGraph
import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.{HttpHeader,HttpRequest,HttpResponse}
import akka.http.scaladsl.model.{StatusCode,StatusCodes}
import akka.stream.Materializer
import cats.Now
import cats.Applicative
import scala.util.Try
import scala.util.Success
import scala.concurrent.{ExecutionContext,Future}


/** 
 * Commands on the Server. All commands are executed agains on a resource
 * named by a URL.
 */ 
sealed trait SolidCmd[A]:
   def url: Uri

object SolidCmd {
	// type NamedGraph = (Uri, Rdf#Graph)
	// type NamedGraphs = Map[Uri,Rdf#Graph]

	type Script[A] = cats.free.Free[SolidCmd,A]

	/**
	 * Get request from URL, but the response should be interpreted to a
	 * manipulatable Scala object. Eg. an RDF Graph for an RDF resource.
	 *
	 * We don't start with a generalised version, to keep things simple.
	 *
	 * ideas: one may want to pass
	 *  - etag, date-since-previous-version,
	 *  - authenticated agent info,
	 *  - content-type?
	 *  - ...
	 * It captures a request that reutrns a Response, where
	 *  k is a function to transform that response into any A
	 **/
	case class Get[A](url: Uri, k: Response => A) extends SolidCmd[A]

	def get(key: Uri): Script[Response] = liftF[SolidCmd, Response](Get[Response](key,identity))

	/**
	 *
	 * @param url to GET
	 * @param k the contination to use once the Response is available
	 * @tparam A
	 * @return The resulting script
	 */
	def getFromPlain[A](
		url: Uri, k: Response => Script[A]
	)(using ExecutionContext, Materializer): Script[A]  =
		import run.cosy.http.{IResponse,RdfParser}
		import scala.util.Failure
		for {
			resp <- SolidCmd.plain(RdfParser.rdfRequest(url))
			tryResp <- resp.status match
				case good: StatusCodes.Success =>
					val fg: Future[IResponse[Rdf#Graph]] = RdfParser.unmarshalToRDF(resp, url)
					//todo: clearly these are close to isomorphic, so we don't need both structures
					val fgres: Future[Response] = fg.map { ir =>
						Response(Meta(ir.origin, ir.status, ir.headers), Success(ir.content))
					}
					SolidCmd.wait[Response](fgres)
				case other =>
					SolidCmd.wait[Response](Future.failed(
						new Throwable("could not parse graph. todo: better error message")))
			x <- k(tryResp.getOrElse(
				Response(Meta(url, StatusCodes.InternalServerError), Failure(new Throwable("todo")))
			))
		} yield x


	/**
	 * This is the base class of commands that covers all the Http Requests.
	 * This is essentially done in terms of 
	 * Execution of such a Request will return an HttpResponse
	 * @param k the continuation function
	 **/
	case class Plain[A](req: HttpRequest, k: HttpResponse => A) extends SolidCmd[A]:
		def url: Uri = req.uri

	def plain(req: HttpRequest): Script[HttpResponse] = liftF[SolidCmd, HttpResponse](Plain(req,identity))

	//todo: if plain2 works, find better name (rename to plain above?).
	def plain2(req: HttpRequest): SolidCmd[Script[HttpResponse]] = Plain(req, Free.pure[SolidCmd,HttpResponse])

	/** Wait that a future has a result. In terms of actors this can use the pipeToSelf command */
	case class Wait[A,B](ftr: Future[A], k: Try[A] => B) extends SolidCmd[B]:
		//todo: The Wait does not have a URL to send it to (or does it always send it to the same actor?)
		def url: Uri = null

	def wait[A](ftr: Future[A]): Script[Try[A]] =
		liftF[SolidCmd, Try[A]](Wait(ftr,identity))

	given CmdFunctor: cats.Functor[SolidCmd] with
		def map[A, B](fa: SolidCmd[A])(f: A => B): SolidCmd[B] = fa match
			//todo: k andThen can lead to stackoverflow. Look at cats.Coyoneda which uses Function.chain
			case Get(url,k) => Get(url, k andThen f)
			case Plain(req,k) => Plain(req, k andThen f)
			case Wait(ftr,k) => Wait(ftr, k andThen f)


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
				cats.free.Free.pure[SolidCmd,ReqDataSet](Cofree(meta, Now(GraF(Graph.empty,Nil))))
	} yield ngs
	
}




