package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpMethods, Uri}
import run.cosy.RDF
import run.cosy.RDF.*
import run.cosy.RDF.ops.*
import run.cosy.ldp.{WebServers,TestServer,ImportsTestServer,TestCompiler}


class TestGuard extends munit.FunSuite {

	import RDF.*
	import RDF.ops.*
	import akka.http.scaladsl.model.HttpMethods.GET
	import cats.implicits.*
	import cats.{Applicative, CommutativeApplicative, Eval, *}
	import run.cosy.http.util.UriX.*
	import run.cosy.ldp.SolidCmd.*



	test("test access to root container resource") {
		val ts = WebServers.imports.server
		val rootAcl = ts.pod / ".acl"
		val rootUri = ts.pod / ""

		val podRdf    = ts.pod.toRdf
		val podRdfAcl = rootAcl.toRdf

		val aclGraph = ts.db(rootAcl)
		assertEquals(Guard.filterRulesFor(aclGraph, rootUri, GET).nodes.toList, List(podRdfAcl.withFragment("Public")))
		val answer = Guard.authorizeScript(rootAcl, new Anonymous(), rootUri, GET).foldMap(WebServers.imports.run)
		assert(answer, true)
	}

}
