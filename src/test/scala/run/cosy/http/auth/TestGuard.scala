package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpMethods, Uri}
import run.cosy.RDF
import run.cosy.RDF.*
import run.cosy.RDF.ops.*


class TestGuard extends munit.FunSuite {

	import RDF.*
	import RDF.ops.*
	import akka.http.scaladsl.model.HttpMethods.GET
	import cats.implicits.*
	import cats.{Applicative, CommutativeApplicative, Eval, *}
	import run.cosy.http.util.UriX.*
	import run.cosy.ldp.LDPCmdTst.*
	import run.cosy.ldp.SolidCmd.*

	val podRdf = pod.toRdf

	test("test access to root container resource") {
		val rootAcl   = pod / ".acl"
		val rootUri   = pod / ""
		val podRdfAcl = rootAcl.toRdf
		val aclGraph  = server(rootAcl)
		assertEquals(Guard.filterRulesFor(aclGraph, rootUri, GET).nodes.toList, List(podRdfAcl.withFragment("Public")))
		val answer = Guard.authorizeScript(rootAcl, new Anonymous(), rootUri, GET).foldMap(simpleCompiler(server))
		assert(answer, true)
	}

}
