package run.cosy.ldp

import akka.http.scaladsl.model.Uri
import cats.free.Cofree
import run.cosy.RDF

class LDPCmdTest extends munit.FunSuite {

	import run.cosy.ldp.*
	import RDF.*
	import RDF.ops.*
	import cats.*
	import cats.implicits.*
	import run.cosy.ldp.SolidCmd.*
	import TestCompiler.*

	import scala.collection.immutable.Set

	// given GraFUnorderedTraverse: cats.UnorderedTraverse[GraF] with
	// 	override
	// 	def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: GraF[A])(f: A => G[B]): G[GraF[B]] =
	// 		sa.other.unorderedTraverse(f).map(ss => sa.copy(other=ss))


	test("fetch included graphs for </People/Berners-Lee/card.acl>") {
		//build the graphs Data structure
		val ts: TestServer = ImportsDLTestServer
		val compiler = TestCompiler(ts)
		val ds: ReqDataSet = fetchWithImports(ts.path("/People/Berners-Lee/card.acl")).foldMap(compiler.eval)

		//count the graphs in ds
		def countGr(ds: ReqDataSet): Eval[Int] = Cofree.cata[GraF, Meta, Int](ds) { (meta, d) =>
			cats.Now(1 + d.other.fold(0)(_ + _))
		}

		assertEquals(countGr(ds).value, 3)

		//return the top NamedGraph of the dataset
		def toNG(ds: ReqDataSet): Eval[(Uri, Rdf#Graph)] = Now(ds.head.url -> ds.tail.value.graph)

		val ts2: TestServer = ConnectedImportsDLTestServer
		val compiler2 = TestCompiler(ts2)

		//check that the output is the same as the full info on the server
		//note this does not keep the structure
		val namedGraphs = ds.coflatMap(toNG).foldLeft[List[(Uri, Rdf#Graph)]](List())((l, b) => b.value :: l)
		assertEquals(Map(namedGraphs.toSeq *), ts.db - ts.path("/People/.acl"))

		//build the graphs Data structure for the altered server
		val ds2: ReqDataSet = fetchWithImports(ts2.path("/People/Berners-Lee/card.acl")).foldMap(compiler2.eval)

		assertEquals(countGr(ds2).value, 4)
		assertEquals(Map(ds2
			.coflatMap(toNG)
			.foldLeft[List[(Uri, Rdf#Graph)]](List())((l, b) => b.value :: l)
			.toSeq *), ts2.db)


		import cats.Now
		// This shows the general structure of the result.
		// We see that there are some arbitrariness as to what the parent of the root acl is: here it is
		//  <https://w3.org/People/Berners-Lee/.acl>
		// The nesting of Graphs makes sense if say the request were fetched via a certain agent (a proxy perhaps).
		// In that case nested graph metadata would really need to include Agent information.
		val urlTree = ds2.mapBranchingS(new (GraF ~> List) {
			def apply[A](gr: GraF[A]): List[A] = gr.other
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
		import WebServers.importsDL.ws
		import WebServers.importsDL.ws.db

		val cardAcl = db(ws.path("/People/Berners-Lee/card.acl"))
		val BLAcl = db(ws.path("/People/Berners-Lee/.acl"))
		// the fixpoints of GF is just a non-empty set of Graphs.
		val g: Graphs = Fix(GraF(cardAcl, List(
			Fix(GraF(BLAcl, List()))))
		)
	}

}