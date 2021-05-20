package run.cosy.ldp

import run.cosy.RDF
import akka.http.scaladsl.model._
import scala.collection.immutable.HashMap
import org.apache.jena.riot.lang.RiotParsers
// import org.w3.banana.{PointedGraph=>PG}

class LDPCmdTst extends munit.FunSuite {
	import run.cosy.ldp._
	import run.cosy.ldp.LDPCmd._
	import scala.collection.immutable.Set
	import RDF.{given,_}
	import RDF.ops.{given,_}

	val owl = org.w3.banana.OWLPrefix[Rdf]
	val wac = org.w3.banana.WebACLPrefix[Rdf]
	val foaf = org.w3.banana.FOAFPrefix[Rdf]

	import cats.implicits._
	// val pg2 = PG[Rdf](URI("/People/Berners-Lee/.acl"),Graph())
	// val pg1 = PG[Rdf](URI("/"),Graph())
	// import org.w3.banana.binder.PGBinder.FromPGToPG2PGBinder
	import org.w3.banana.binder.ToNode
	import org.w3.banana.binder.ToNode.{given,_}

	//see https://github.com/lampepfl/dotty/discussions/12527
	implicit def URIToNode: ToNode[Rdf,Rdf#URI] = new ToNode[Rdf, Rdf#URI] {
		def toNode(t: Rdf#URI): Rdf#Node = t
	}
	implicit def BNodeToNode: ToNode[Rdf,Rdf#BNode] = new ToNode[Rdf, Rdf#BNode] {
		def toNode(t: Rdf#BNode): Rdf#Node = t
	}

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
	)) union w3c(URI("/People/Berners-Lee/.acl") -- owl.imports ->- URI("/.acl"))

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
	import cats.{Id, ~>}

	def simpleCompiler: LDPCmd ~> Id = new (LDPCmd ~> Id) {
		val server: Map[Uri, Rdf#Graph] = Map(
			w3cu("/.acl") -> rootACL,
			w3cu("/People/.acl") -> pplAcl,
			w3cu("/People/Berners-Lee/.acl") -> BLAcl,
			w3cu("/People/Berners-Lee/card.acl") -> cardAcl
		)

		def apply[A](cmd: LDPCmd[A]): Id[A] = 
			cmd match {
				case Get(url) => server.get(url).asInstanceOf[A]
			}
	}
		
	test("fetch included graph for </People/Berners-Lee/card.acl>") {
		val graphs = fetchWithImports(w3cu("/People/Berners-Lee/card.acl")).foldMap(simpleCompiler)
		assertEquals(graphs.size,3)
		assertEquals(graphs.map(_._1).toSet, 
			Set(w3cu("/.acl"),w3cu("/People/Berners-Lee/.acl"), w3cu("/People/Berners-Lee/card.acl")))
	}

	test("fetch included graph for </People/.acl>") {
		val graphs = fetchWithImports(w3cu("/People/.acl")).foldMap(simpleCompiler)
		assertEquals(graphs.size,2)
		assertEquals(graphs.map(_._1).toSet, 
			Set(w3cu("/.acl"),w3cu("/People/.acl")))
	}


}