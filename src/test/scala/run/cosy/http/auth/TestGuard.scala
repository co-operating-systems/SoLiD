package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpMethods, Uri}
import run.cosy.RDF
import run.cosy.RDF.*
import run.cosy.RDF.ops.*
import run.cosy.ldp.{WebServers,TestServer,ImportsDLTestServer,TestCompiler}


class TestGuard extends munit.FunSuite {

	import RDF.*
	import RDF.ops.*
	import akka.http.scaladsl.model.HttpMethods.{GET,PUT,POST,DELETE}
	import cats.implicits.*
	import cats.{Applicative, CommutativeApplicative, Eval, *}
	import run.cosy.http.util.UriX.*
	import run.cosy.ldp.SolidCmd.*

	test("test access to root container resource for simple ACL server") {
		import WebServers.aclBasic
		import aclBasic.ws
		val rootAcl = ws.base / ".acl"
		val rootUri = ws.base / ""

		val podRdfAcl = rootAcl.toRdf

		val owner = (ws.base / "owner").withFragment("i")
		val timBlCardUri = ws.base / "People" /"Berners-Lee"/"card"

		val aclGraph = ws.db(rootAcl)
		assertEquals(Guard.filterRulesFor(aclGraph, rootUri, GET).nodes.toList,
			List(podRdfAcl.withFragment("Public"), podRdfAcl.withFragment("Admin")))
		val answer = Guard.authorizeScript(rootAcl, new Anonymous(), rootUri, GET).foldMap(aclBasic.eval)
		assert(answer, "The root acl allows all contents to be READable")

		assert(!Guard.authorize(aclGraph,Anonymous(),rootUri,POST), "Anonymous should not have POST access")
		val answer2Post = Guard.authorizeScript(rootAcl, Anonymous(), rootUri, POST).foldMap(aclBasic.eval)
		assert(!answer2Post, "Anonymous should not have POST access to root container" )

		val answer2Post2 = Guard.authorizeScript(rootAcl, WebIdAgent(owner), rootUri, POST).foldMap(aclBasic.eval)
		assert(answer2Post2, "The owner should have POST access to root container" )
		
		val answer3 = Guard.authorizeScript(rootAcl, new Anonymous(), timBlCardUri, GET).foldMap(aclBasic.eval)
		assert(answer3, "Anyyone can read Tim Berners-Lee's card")
		val answer3PUT = Guard.authorizeScript(rootAcl, WebIdAgent(owner), timBlCardUri, GET).foldMap(aclBasic.eval)
		assert(answer3, "The owner can PUT to Tim Berners-Lee's card")

		val answer4 = Guard.authorizeScript(rootAcl, new Anonymous(), timBlCardUri, PUT).foldMap(aclBasic.eval)
		assert(!answer4, "The root rule applies by default to Tim Berners-Lee's card, disallowing PUT by anonymous")
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
