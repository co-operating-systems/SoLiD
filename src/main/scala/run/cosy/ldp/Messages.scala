package run.cosy.ldp

import akka.actor.typed.ActorRef
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.Materializer
import run.cosy.http.auth.Agent
import scalaz.{ICons, INil, NonEmptyList}
import scalaz.NonEmptyList.nel

import java.nio.file.Path
import scala.concurrent.ExecutionContext

object Messages {
	sealed trait Cmd

	//trait for a Cmd that is to be acted upon
	sealed trait Act extends Cmd
	sealed trait Route extends Cmd
	sealed trait Info extends Cmd

	sealed trait ReqMsg {
		val req: HttpRequest
		val from: Agent
		val replyTo: ActorRef[HttpResponse]
	}

	/**
	 * @param path the path to the final resource */
	final case class RouteMsg(
		path: NonEmptyList[String],
		from: Agent,
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqMsg with Route {
		def name: String = path.head

		// check that path is not empty before calling  (anti-pattern)
		def next: ReqMsg with Route = path match
			case NonEmptyList(_, INil()) => WannaDo(from, req, replyTo)
			case NonEmptyList(_, ICons(h,tail)) => RouteMsg(nel(h,tail), from, req, replyTo)
	}

	/** a command to be executed on the resource on which it arrives, after being authorized */
	final case class Do(
		from: Agent,
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqMsg with Act

	/** message has arrived, but still needs to be authorized */
	final case class WannaDo(
		from: Agent,
		req: HttpRequest,
		replyTo: ActorRef[HttpResponse]
	)(using val mat: Materializer, val ec: ExecutionContext) extends ReqMsg with Route
	
	// responses from child to parent
	case class ChildTerminated(name: String) extends Info

}
