package run.cosy.http.auth

import akka.http.scaladsl.model.{HttpMethod, Uri}
import org.w3.banana.{PointedGraphs, WebACLPrefix}
import run.cosy.ldp.SolidCmd.ReqDataSet
import akka.actor.typed.scaladsl.ActorContext
import run.cosy.ldp.Messages.CmdMessage

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

object Guard {

	import akka.actor.typed.scaladsl.Behaviors
	import akka.http.scaladsl.model.HttpMethods.*
	import akka.http.scaladsl.model.headers.{`WWW-Authenticate`, HttpChallenge}
	import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
	import org.w3.banana.diesel.PointedGraphW
	import org.w3.banana.PointedGraph
	import org.w3.banana.syntax.{GraphW, NodeW}
	import run.cosy.RDF.*
	import run.cosy.RDF.ops.*
	import run.cosy.RDF.Prefix.{rdf, security, wac}
	import run.cosy.http.util.UriX.*
	import run.cosy.ldp.Messages.{Do, ScriptMsg, WannaDo}
	import run.cosy.ldp.SolidCmd.{fetchWithImports, unionAll, Get, Plain, Script, Wait}
	import run.cosy.ldp.SolidCmd

	import scala.util.{Failure, Success}

	type GMethod = GET.type | PUT.type | POST.type | DELETE.type //| PATCH.type

	/**
	 * Determine if an agent is authorized to request the operation on the target resource,
	 * given the graph of ACL rules.
	 *
	 * Note that as discussed in [[https://github.com/solid/authorization-panel/discussions/223 Thinking in terms of proofs and Origins]]  a more sophisticated implementation may want to
	 * also know more about the proof that `agent` is making the request: e.g. was the agent talking via
	 * another Origin? We may also end up with not being given an Agent but a partial description of an Agent as given
	 * by a certificate, and where one would need to verify if that partial description fit a rule.
	 *
	 * Here we assume all the data is in the queried graph. Which will lead to a lot of duplication.
	 * More sophisticated rules, will require some data to be fetched outside the graph (such as to get foaf:knows relations)
	 * foaf:group, agentClass, link from an acl?
	 * To deal with those graphs correctly, we need a DataSet, but we may only want to fetch those
	 * when needed, which indicates we may want to work with a Script[ReqDataSet]...
	 *
	 * Because this is all in one graph it could actually be implemented as a SPARQL query.
	 */
	def authorize(acg: Rdf#Graph, agent: Agent, target: Uri, operation: GMethod): Boolean =
		(filterRulesFor(acg, target, operation) / wac.agent).exists { pg =>
			agent match
				case KeyIdAgent(keyId, _) => pg.graph.select(keyId.toRdf, security.controller, pg.pointer).hasNext
				case _ => false
		}

	/**
	 * from a graph of rules, return a stream of pointers to the rules that apply to the given target
	 * and operation.
	 * The only description supported to start with is acl:default, and we test this by substring the URL.
	 * todo: we don't deal with rules that specify the target by description, which would require looking
	 * up metadata on the target URI to check if it matches the description.
	 * Doing that would require having information for the target resource, or fetching such information...
	 * Fetching would mean this would need to be a Script?
	 *
	 * @param acRulesGraph
	 */
	def filterRulesFor(acRulesGraph: Rdf#Graph, target: Uri, operation: GMethod): PointedGraphs[Rdf] =
		val targetRsrc: Rdf#URI = target.toRdf
		//we assume that all rules are typed
		val rules: PointedGraphs[Rdf] = PointedGraph[Rdf](wac.Authorization, acRulesGraph) /- rdf.`type`
		//todo: add filter to banana-rdf 
		val it: Iterable[Rdf#Node] = rules.nodes.filter { (node: Rdf#Node) =>
			acRulesGraph.find(node, wac.mode, modeFor(operation)).hasNext &&
				(acRulesGraph.select(node, wac.accessTo, targetRsrc).hasNext ||
					acRulesGraph.select(node, wac.default, ANY).exists { (tr: Rdf#Triple) =>
						tr.subject.fold(u => u.toAkka.ancestorOf(target), x => false, z => false)
					})
		}
		PointedGraphs(it, acRulesGraph)

	def modeFor(op: GMethod): Rdf#URI =
		import run.cosy.ldp.SolidCmd
		op match
			case GET => wac.Read
			case PUT => wac.Write
			case POST => wac.Write
			case DELETE => wac.Write
	//case PATCH => wac.Write

	def authorizeScript(aclUri: Uri, agent: Agent, target: Uri, method: HttpMethod): Script[Boolean] =
		import akka.actor.typed.Behavior
		for {
			reqDS <- fetchWithImports(aclUri)
		} yield 
			method match
				case m: GMethod => authorize(unionAll(reqDS),agent,target,m)
				case _ => false

	/** we authorize the top command `msg` - it does not really matter what T the end result is. */
	def Authorize[T](msg: CmdMessage[T], aclUri: Uri)(using
		context: ActorContext[ScriptMsg[_]|Do]
	): Unit =
		//todo!!: set timeout in config!!
		given timeOut : akka.util.Timeout = akka.util.Timeout(Duration(3,TimeUnit.SECONDS))
		context.log.info(s"Authorize(WannaDo($msg), <$aclUri>)")
		import SolidCmd.{Get, Wait}
		import run.cosy.http.auth.Guard.*
		import cats.free.Free.pure
		if msg.from == WebServerAgent then
			context.self ! Do(msg)
		else
			msg.commands match
			case p: Plain[_] =>
				import msg.given
				//this script should be sent with admin rights.
				// note: it could also be sent with the rights of the user, meaning that if the user cannot
				//   read an acl rule, then it has no access. But that would slow things down as it would require
				//   every request on every acl to be access controlled!
				context.ask[ScriptMsg[Boolean],Boolean](
					context.self,
					ref => ScriptMsg[Boolean](
						authorizeScript(aclUri, msg.from, msg.target, p.req.method), WebServerAgent, ref)
				){
					case Success(true) =>  Do(msg)
					case Success(false) => msg.respondWithScr(HttpResponse(StatusCodes.Unauthorized,
							Seq(`WWW-Authenticate`(HttpChallenge("Signature",s"${msg.target}")))
					))
					case Failure(e) => msg.respondWithScr(HttpResponse(StatusCodes.Unauthorized,
							Seq(`WWW-Authenticate`(HttpChallenge("Signature",s"${msg.target}"))),
							HttpEntity(ContentTypes.`text/plain(UTF-8)`,e.getMessage))
					)
				}
			case _ => //the other messages end up getting translated to Plain reuests . todo: check
				context.self ! Do(msg)



}
