package run.cosy.ldp

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop}
import akka.http.scaladsl.model._
import run.cosy.ldp

import java.nio.file.{Files, Path => FPath}
import scala.collection.immutable.HashMap

/**
 * LDPC Actor extended with Web Access Control and other SoLiD features.
 */
object Container { //extends LDPRActor(ldpcUri,root) {

	import HttpMethods._
	import StatusCodes._
	// log.info(s"created LDPC($ldpcUri,$root)")

	def apply(ldpcUri: Uri,
				 root: FPath,
				 contains: Map[String, ActorRef[Cmd]] = HashMap()): Behavior[Cmd] = {
		Behaviors.setup[Cmd] { context =>
			val registry = ldp.ContainerRegistry(context.system)
			registry.addActorRef(ldpcUri.path, context.self)
			context.log.info("started actor at "+ldpcUri.path)
			Behaviors.receiveMessage[Cmd] { msg =>
				def processHttpReq(msg: RouteHttp): Behavior[Cmd] = {
					msg.replyTo ! {
						if msg.req.uri.path == ldpcUri.path then
							HttpResponse(OK, entity = s"Container received ${msg.req.uri}")
						else
							HttpResponse(NotFound, entity = s"could not find ${msg.req.uri}")
					}
					Behaviors.same
				}

				def routeHttpReq(msg: RouteHttp): Behavior[Cmd] = {
					context.log.info(s"received msg: ${msg.path} in ${root.toFile.getAbsolutePath} in container with uri $ldpcUri" )

					msg.path match
						case Nil => processHttpReq(msg)
						case head :: tail =>
							contains.get(head) match
								case Some(ref) => ref ! msg.next
								case None => msg.req match
									case HttpRequest(PUT, _, _, _, _) =>
										//special case because it could be requesting to make intermediary subdirectories
										msg.replyTo ! HttpResponse(MethodNotAllowed)
									case other =>
										val file = root.resolve(head)
										if Files.exists(file) then
											val newref = if Files.isDirectory(file) then
												context.log.info(s"Creating container for ${ldpcUri.path/head} at $file")
												val ref = context.spawn(Container(ldpcUri.withPath(ldpcUri.path / head), file), head)
												context.watchWith(ref, ChildTerminated(head))
												ref ! msg.next
												Container(ldpcUri, root, contains + (head -> ref))
											else
												val nr: ActorRef[RouteHttp] = context.spawn(
													Resource(ldpcUri.withPath(ldpcUri.path / head), file, head),
													head)
												nr ! msg.next
										else
											msg.replyTo ! HttpResponse(NotFound,
												entity=s"file $file does not exist in ${root.toFile.getAbsolutePath}")
							Behaviors.same
				}

				msg match
					case routeMsg: RouteHttp => routeHttpReq(routeMsg)
					case ChildTerminated(name) => 
						registry.removePath(ldpcUri.path/name)
						apply(ldpcUri, root, contains - name)

			}
			// we don't need this yet, as we are not shutting down the Container Actor yet.
//			  .receiveSignal {
//				case (ctx, PostStop) =>
//					registry.removePath(ldpcUri.path)
//					Behaviors.stopped
//			}
		}
	}

	sealed trait Cmd

	/** @param path the path to the container */
	final case class RouteHttp(path: List[String], req: HttpRequest, replyTo: ActorRef[HttpResponse]) extends Cmd {
		// check that path is not empty before calling
		def next = RouteHttp(path.tail, req, replyTo)
	}

	private case class ChildTerminated(name: String) extends Cmd


}




