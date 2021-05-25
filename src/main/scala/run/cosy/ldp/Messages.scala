package run.cosy.ldp

import akka.actor.typed.ActorRef
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model._
import akka.stream.Materializer
import run.cosy.http.auth.Agent
import scalaz.{ICons, INil, NonEmptyList}
import scalaz.NonEmptyList.nel

import java.nio.file.Path
import scala.concurrent.ExecutionContext

object Messages {
	sealed trait Cmd

	//trait for a Cmd that is to be acted upon
	sealed trait Act extends Cmd {
		def msg: ReqMsg
	}
	sealed trait Route extends Cmd {
		def msg: ReqMsg
	}
	sealed trait Info extends Cmd

	sealed trait ReqMsg {
		type Request
		type Response
		val req: Request
		val from: Agent
		val replyTo: ActorRef[Response]
		def returnError(res: HttpResponse): Unit
		def redirectTo(uri: Uri, msg: String): Unit
		def uri: Uri
	}

	case class HttpReqMessage(req: HttpRequest, from: Agent, replyTo: ActorRef[HttpResponse]) extends ReqMsg {
		type Request = HttpRequest
		type Response = HttpResponse
		def uri: Uri = req.uri
		def returnError(res: HttpResponse): Unit = replyTo ! res
		def redirectTo(redirectTo: Uri, msg: String): Unit = 
			replyTo ! HttpResponse(MovedPermanently, Seq(Location(redirectTo)), msg)
	}

	import LDPCmd.ReqDataSet
	case class ScriptReqMessage[T](script: LDPCmd.LDPScript[T], from: Agent, replyTo: ActorRef[T]) extends ReqMsg {
		type Request = LDPCmd.LDPScript[T]
		type Response = T
		def uri: Uri = script.resume match 
			case Left(cmd) => ???
			case Right(nextScr) => ???

		def returnError(res: HttpResponse): Unit = 
			??? //todo: what should an error do? Should it apply the HttpResponse as error to the cmd?
		def redirectTo(uri: Uri, msg: String): Unit
	}

	/**
	 * @param path the path to the final resource */
	final case class RouteMsg(
		path: NonEmptyList[String],
		msg: ReqMsg
	)(using val mat: Materializer, val ec: ExecutionContext) extends Route {
		def name: String = path.head

		// check that path is not empty before calling  (anti-pattern)
		def next: Route = path match
			case NonEmptyList(_, INil()) => WannaDo(msg)
			case NonEmptyList(_, ICons(h,tail)) => RouteMsg(nel(h,tail), msg)
	}

	/** a command to be executed on the resource on which it arrives, after being authorized */
	final case class Do(
		msg: ReqMsg
	)(using val mat: Materializer, val ec: ExecutionContext) extends Act

	/** message has arrived, but still needs to be authorized */
	final case class WannaDo(
		 msg: ReqMsg
	)(using val mat: Materializer, val ec: ExecutionContext) extends Route {
		def toDo = Do(msg)
	}
	
	// responses from child to parent
	case class ChildTerminated(name: String) extends Info

}
