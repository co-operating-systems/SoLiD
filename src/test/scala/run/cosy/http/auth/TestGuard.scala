package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpMethods, Uri}
import run.cosy.RDF
import run.cosy.RDF.*
import run.cosy.RDF.ops.*
import run.cosy.ldp.{WebServers,TestServer,ImportsDLTestServer,TestCompiler}


class TestGuard extends munit.FunSuite {

	import RDF.*
	import RDF.ops.*
	import akka.http.scaladsl.model.HttpMethods.GET
	import cats.implicits.*
	import cats.{Applicative, CommutativeApplicative, Eval, *}
	import run.cosy.http.util.UriX.*
	import run.cosy.ldp.SolidCmd.*

	test("test access to root container resource for simple ACL server") {
		import WebServers.aclBasic
		import aclBasic.ws
		val rootAcl = ws.base / ".acl"
		val rootUri = ws.base / ""

		val podRdf    = ws.base.toRdf
		val podRdfAcl = rootAcl.toRdf

		val aclGraph = ws.db(rootAcl)
		assertEquals(Guard.filterRulesFor(aclGraph, rootUri, GET).nodes.toList, List(podRdfAcl.withFragment("Public")))
		val answer = Guard.authorizeScript(rootAcl, new Anonymous(), rootUri, GET).foldMap(aclBasic.eval)
		assert(answer, true)
	}



	test("test access to root container resource") {
		import WebServers.importsDL
		import importsDL.ws
		val rootAcl = ws.base / ".acl"
		val rootUri = ws.base / ""

		val podRdf    = ws.base.toRdf
		val podRdfAcl = rootAcl.toRdf

		val aclGraph = ws.db(rootAcl)
		assertEquals(Guard.filterRulesFor(aclGraph, rootUri, GET).nodes.toList, List(podRdfAcl.withFragment("Public")))
		val answer = Guard.authorizeScript(rootAcl, new Anonymous(), rootUri, GET).foldMap(importsDL.eval)
		assert(answer, true)
	}

}
