
package run.cosy.ldp

import run.cosy.RDF
import RDF.{given,_}
import RDF.ops.{given,_}
import akka.http.scaladsl.model.Uri
import cats.free.Free
import cats.free.Free.liftF
import org.apache.jena.sparql.core.NamedGraph

sealed trait LDPCmd[A]
case class Get[T](url: Uri) extends LDPCmd[Option[T]]

type LDPScript[A] = Free[LDPCmd,A]

object LDPCmd {
	type NamedGraph = (Uri, Rdf#Graph)
	type NamedGraphs = Map[Uri,Rdf#Graph]



	/**
	 * LDP DataSets, where Graphs are named by URLs and retrievable from that location. 
	 * The first URI is the default graph.
	 */
	type DataSet = (Uri,NamedGraphs)

	def get[T](key: Uri): LDPScript[Option[T]] = liftF[LDPCmd, Option[T]](Get[T](key))

	/**
	 * Build a Script to fetch owl:imports links starting from a graph at u, avoiding visted ones.
	 * See [[https://github.com/solid/authorization-panel/issues/210 Issue 210 on :imports]], though here
	 * we use owl.imports provisionally until a better name is chosen.
	 * 
	 * The script returns a list of named graphs.
	 * It is quite possible that different branches could fetch the same graphs, but it should not lead to infinite loops.
	 * todo: there is parallelism here when fetching children: would it work to use Applicatives?
	 * todo: returning a Set may be better, but I had trouble getting Traverse to work on Set
	 */
	def fetchWithImports(u: Uri, visited: Set[Uri]=Set()) : LDPScript[List[NamedGraph]] = for {
			gOpt <- get[Rdf#Graph](u)
			ngs: List[NamedGraph] <- gOpt match 
				case Some(g) => 
					import cats.syntax.all.toTraverseOps
					val imports: List[Uri] = find(g, ANY, owl.imports, ANY).toList.collect{ case Triple(_,_,o: Rdf#URI) => o.toAkka }
					val newVisited = visited+u
					val newImports = imports.filterNot(newVisited.contains(_))
					val covered = newVisited ++ newImports
					val lstOfScrpDs: List[LDPScript[List[NamedGraph]]] =  newImports.map(u => fetchWithImports(u, covered))
					lstOfScrpDs.flatSequence.map(lng => u -> g :: lng)
				case None => cats.free.Free.pure[LDPCmd,List[NamedGraph]](List())
		} yield ngs
	
}




