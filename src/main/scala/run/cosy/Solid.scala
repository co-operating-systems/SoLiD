package run.cosy

import akka.Done
import akka.actor.{CoordinatedShutdown}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, LoggerOps}
import akka.actor.typed.{Behavior, ActorRef, ActorSystem, Scheduler, PostStop}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.Uri.Path.{Empty, Segment, Slash}
import akka.http.scaladsl.model
import akka.http.scaladsl.model.{HttpResponse, Uri}
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.{RequestContext, Route, RouteResult}
import akka.http.scaladsl.util.FastFuture
import akka.util.Timeout
import cats.data.NonEmptyList
import com.typesafe.config.{Config, ConfigFactory}
import run.cosy.ldp.{FSContainer, ResourceRegistry}

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success}


object Solid {
	//todo: make @tailrec
	def pathToList(path: Uri.Path): List[String] = path match {
		case Empty => Nil
		case Segment(head, tail) => head :: pathToList(tail)
		case Slash(tail) => pathToList(tail) //note: ignore slashes. But could they be useful?
	}

	def apply(uri: Uri, fpath: Path): Behavior[Run] =
		Behaviors.setup { (ctx: ActorContext[Run]) =>
			given system: ActorSystem[Nothing] = ctx.system
			given reg : ResourceRegistry = ResourceRegistry(ctx.system)
			val withoutSlash = uri.withPath(uri.path.reverse.dropChars(1).reverse)
			val rootRef: ActorRef[FSContainer.Cmd] = ctx.spawn(FSContainer(withoutSlash, fpath), "solid")
			val registry = ResourceRegistry(system)
			val solid = new Solid(fpath, uri, registry, rootRef)
			given timeout: Scheduler = system.scheduler
			given ec: ExecutionContext = ctx.executionContext


			val serverBinding = Http()
				.newServerAt(uri.authority.host.address(), uri.authority.port)
				.bind(solid.route)
        
			ctx.pipeToSelf(serverBinding) {
				case Success(binding) =>
					val shutdown = CoordinatedShutdown(system)

					shutdown.addTask(CoordinatedShutdown.PhaseServiceUnbind, "http-unbind") { () =>
						binding.unbind().map(_ => Done)
					}
					import concurrent.duration.DurationInt
					shutdown.addTask(CoordinatedShutdown.PhaseServiceRequestsDone, "http-graceful-terminate") { () =>
						binding.terminate(10.seconds).map(_ => Done)
					}
					shutdown.addTask(CoordinatedShutdown.PhaseServiceStop, "http-shutdown") { () =>
						Http().shutdownAllConnectionPools().map(_ => Done)
					}	
					Started(binding)
				case Failure(ex) => StartFailed(ex)
			}

			
			
			def running(binding: ServerBinding): Behavior[Run] =
				Behaviors.receiveMessagePartial[Run] {
					case Stop =>
						ctx.log.info(
							"Stopping server http://{}:{}/",
							binding.localAddress.getHostString,
							binding.localAddress.getPort)
						Behaviors.stopped
				}.receiveSignal {
					case (_, PostStop) =>
						binding.unbind()
						Behaviors.same
				}

			def starting(wasStopped: Boolean): Behaviors.Receive[Run] =
				Behaviors.receiveMessage[Run] {
					case StartFailed(cause) =>
						throw new RuntimeException("Server failed to start", cause)
					case Started(binding) =>
						ctx.log.info("Server online at http://{}:{}/",
							binding.localAddress.getHostString,
							binding.localAddress.getPort)
						if (wasStopped) ctx.self ! Stop
						running(binding)
					case Stop =>
						// we got a stop message but haven't completed starting yet,
						// we cannot stop until starting has completed
						starting(wasStopped = true)
				}

			starting(wasStopped = false)
		}

	//question: could one use ADTs instead?
	sealed trait Run
	case class StartFailed(cause: Throwable) extends Run
	case class Started(binding: ServerBinding) extends Run
	case object Stop extends Run

	//ActorSystem.t
	// where was this?

}

/**
 * The object from from which the solid server is called. 
 * This object also keeps track of actorRef -> path mappings
 * so that the requests can go directly to the right actor for a resource
 * once all the intermediate containers have been set up. 
 *
 * We want to have intermediate containers so that some can be setup
 * to read from the file system, others from git or CVS, others yet from 
 * a DB, ... A container actor would know what behavior it implements by 
 * looking at some config file in that directory.
 *
 * This is an object that can be called simultaneously by any number
 * of threads, so all state in it should be protected by Atomic locks.
 *
 * @param path    to the root directory of the file system to be served
 * @param baseUri of the container, eg: https://alice.example/solid/
 */
class Solid(path: Path,
            baseUri: Uri,
            registry: ResourceRegistry,
            rootRef: ActorRef[FSContainer.Cmd])(using sys: ActorSystem[_]) {

	import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}
	import akka.pattern.ask

	import scala.concurrent.duration.*
	import scala.jdk.CollectionConverters.*
	given timeout: Scheduler = sys.scheduler
	given scheduler: Timeout = Timeout(5.second)
	

	lazy val route: Route = (reqc: RequestContext) => {
		val path = reqc.request.uri.path
		import reqc.{given}
		reqc.log.info("routing req " + reqc.request.uri)
		val (remaining, actor): (List[String], ActorRef[FSContainer.Cmd]) = registry.getActorRef(path)
			.getOrElse((List[String](), rootRef))
		println("remaining=" + remaining)
		def cmdFn(ref: ActorRef[HttpResponse]): FSContainer.Cmd = remaining match {
			case Nil =>  FSContainer.Do(reqc.request,ref)
			case head::tail => FSContainer.Route(NonEmptyList(head,tail), reqc.request, ref)
		}
		actor.ask[HttpResponse](cmdFn).map(RouteResult.Complete(_))
	}


	//  def handle(request: HttpRequest): Future[HttpResponse] = {
	//    // todo: what does the type of this message have to be to receive a HttpResponse?
	//      registry.getActorRef(request.uri.path) match {
	//        case Some(ref) => ctxt.ask(ref) toMessage(request).asInstanceOf[Future[HttpResponse]]
	//        case None => Future.successful(HttpResponse(StatusCodes.ServiceUnavailable))  
	//      }


}

