package run.cosy

import org.w3.banana.jena.Jena
import akka.http.scaladsl.model.Uri
/**
 * Set your preferred implementation of banana-rdf here.
 * Note: this way of setting a preferred implementation of RDF means that
 * all code referring to this must use one implementation of banana at compile time.
 * A more flexible but more tedious  approach, where different parts of the code
 * could use different RDF implementations, and interact via translations, would require all the
 * code to take Rdf and ops implicit parameters `given` with the `using` keyword.
 *
 **/
object RDF {
	export Jena.*
	export Jena.given

	extension (uri: Uri)
		def toRdf: Rdf#Node = ops.URI(uri.toString)

}

