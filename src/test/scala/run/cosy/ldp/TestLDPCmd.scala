package run.cosy.ldp

import run.cosy.RDF
import akka.http.scaladsl.model.*

import scala.collection.immutable.HashMap
import org.apache.jena.riot.lang.RiotParsers
import cats.free.Cofree

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success
// import org.w3.banana.{PointedGraph=>PG}

class LDPCmdTst extends munit.FunSuite {
	import run.cosy.ldp._
	import run.cosy.ldp.SolidCmd._
	import scala.collection.immutable.Set
	import RDF.{given,_}
	import RDF.ops.{given,_}

	val owl = org.w3.banana.OWLPrefix[Rdf]
	val wac = org.w3.banana.WebACLPrefix[Rdf]
	val foaf = org.w3.banana.FOAFPrefix[Rdf]

	import cats.implicits._

	def w3cu(u: String): Uri = Uri("https://w3.org").withPath(Uri.Path(u))

	def w3c(pg: org.w3.banana.PointedGraph[Rdf]): Rdf#Graph = pg.graph.resolveAgainst(URI("https://w3.org"))

	val containedIn = URI("http://ont.example/containedIn")

	// let's put together the example illustrated in 
	// https://github.com/solid/authorization-panel/issues/210#issuecomment-838747077
	val cardAcl = w3c(URI("/People/Berners-Lee/card.acl") -- owl.imports ->- URI("/People/Berners-Lee/.acl"))

	val BLAcl = w3c(URI("/People/Berners-Lee/.acl#TimRl") -- wac.agent ->- ( 
		URI("/People/Berners-Lee/card#i") -- wac.accessToClass ->- ( 
			bnode() -- containedIn ->- URI("/People/Berners-Lee/")
		)
	)) union w3c(
		URI("/People/Berners-Lee/.acl") -- owl.imports ->- URI("/.acl")
	)

	// we also want to consider the world where the full import hierarchy is preserved
	val BLAcl2 = BLAcl union w3c(
			URI("/People/Berners-Lee/.acl") -- owl.imports ->- URI("/People/.acl")
		)

	val pplAcl = w3c(
		URI("/People/.acl#AdminRl") 
			-- wac.mode ->- wac.Control
			-- wac.agent ->- URI("/People/HR#i")
			-- wac.accessToClass ->- (bnode() -- containedIn ->- URI("/People/"))
		) union w3c(URI("/People/.acl") -- owl.imports ->- URI("/.acl"))

	val rootACL = w3c(URI("/.acl#Admin")
		-- wac.mode ->- wac.Control
		-- wac.agentClass ->- URI("/Admins#g")
		-- wac.accessToClass ->- bnode("allContents")).graph union 
			w3c(URI("/.acl#Public") -- wac.accessToClass ->- bnode("allContents")
				-- wac.mode ->- wac.Read
				-- wac.agentClass ->- foaf.Agent) 

	import cats.arrow.FunctionK
	import cats.catsInstancesForId
	import cats.{Id, ~>, Now}
	
	val server: Map[Uri, Rdf#Graph] = Map(
			w3cu("/.acl") -> rootACL,
			w3cu("/People/.acl") -> pplAcl,
			w3cu("/People/Berners-Lee/.acl") -> BLAcl,
			w3cu("/People/Berners-Lee/card.acl") -> cardAcl
		)

	val server2 = server + (w3cu("/People/Berners-Lee/.acl") -> BLAcl2)

	import akka.http.scaladsl.model.StatusCodes
	given ec : scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
	def simpleCompiler(db: Map[Uri, Rdf#Graph]): SolidCmd ~> Id = new (SolidCmd ~> Id) {	
		def apply[A](cmd: SolidCmd[A]): Id[A] =  cmd match 
			case Get(url,f) => db.get(url) match 
				case Some(g) => f(Response(Meta(url,StatusCodes.OK,Seq()),Success(g)))
				//todo: Create an exception for this that can be re-used
				case None => f(Response(Meta(url,StatusCodes.NotFound,Seq()),Failure(new Exception("no content"))))
			case Plain(_, _) => ??? //todo: built up an example with Plain contents
			case Wait(future, uri, k) =>
				Await.result(future.transform(atry => Success(k(atry))), Duration(20,TimeUnit.SECONDS))
	}

	import cats.{Applicative,CommutativeApplicative, Eval}
	// given GraFUnorderedTraverse: cats.UnorderedTraverse[GraF] with  
	// 	override 
	// 	def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: GraF[A])(f: A => G[B]): G[GraF[B]] = 
	// 		sa.other.unorderedTraverse(f).map(ss => sa.copy(other=ss))
	
			
	test("fetch included graphs for </People/Berners-Lee/card.acl>") {
      //build the graphs Data structure
		val ds: ReqDataSet = fetchWithImports(w3cu("/People/Berners-Lee/card.acl")).foldMap(simpleCompiler(server))

		//count the graphs in ds
		def countGr(ds: ReqDataSet): Eval[Int] = Cofree.cata[GraF,Meta,Int](ds){(meta, d) =>
			cats.Now( 1 + d.other.fold(0)(_+_)) }
		assertEquals(countGr(ds).value,3)

		//return the top NamedGraph of the dataset
		def toNG(ds: ReqDataSet): Eval[(Uri,Rdf#Graph)] = Now(ds.head.url -> ds.tail.value.graph)

		
		//check that the output is the same as the full info on the server
		//note this does not keep the structure
		val namedGraphs = ds.coflatMap(toNG).foldLeft[List[(Uri,Rdf#Graph)]](List())((l, b) => b.value::l)
		assertEquals(Map(namedGraphs.toSeq*),server - w3cu("/People/.acl"))

      //build the graphs Data structure for the altered server
		val ds2: ReqDataSet = fetchWithImports(w3cu("/People/Berners-Lee/card.acl")).foldMap(simpleCompiler(server2))
		
		assertEquals(countGr(ds2).value,4)
		assertEquals(Map(ds2.coflatMap(toNG).foldLeft[List[(Uri,Rdf#Graph)]](List())((l, b) => b.value::l).toSeq*),server2)

		
		import cats.Now
		// This shows the general structure of the result. 
		// We see that there are some arbitrariness as to what the parent of the root acl is: here it is
		//  <https://w3.org/People/Berners-Lee/.acl> 
		// The nesting of Graphs makes sense if say the request were fetched via a certain agent (a proxy perhaps). 
		// In that case nested graph metadata would really need to include Agent information.
		val urlTree = ds2.mapBranchingS(new (GraF ~> List) { 
			def apply[A](gr: GraF[A]): List[A]  = gr.other 
		}).map(_.url).forceAll
		
		assertEquals(urlTree, 
				Cofree(Uri("https://w3.org/People/Berners-Lee/card.acl"),
					Now(List(Cofree(Uri("https://w3.org/People/Berners-Lee/.acl"),
						Now(List(Cofree(Uri("https://w3.org/People/.acl"), Now(Nil)),
							Cofree(Uri("https://w3.org/.acl"), Now(Nil)))))))))
		//we want to see if the nesting is correct, even if we don't really need the nesting at this point
	}


	test("What is a Fix[GraF]?") {
	 	//simple Fix as as defined in http://tpolecat.github.io/presentations/cofree/slides#19 
		case class Fix[F[_]](f: F[Fix[F]])
		type Graphs = Fix[GraF]
		// the fixpoints of GF is just a non-empty set of Graphs.
		val g: Graphs = Fix(GraF(cardAcl,List(
				Fix(GraF(BLAcl,List()))))
			)
	}

}