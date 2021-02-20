package run.cosy.ldp

import akka.actor.typed.scaladsl.Behaviors

import java.nio.file.{Path => FPath}
import java.security.Principal
import akka.actor.typed.ActorRef
import akka.http.scaladsl.model._
import run.cosy.ldp.Container.RouteHttp
import akka.actor.typed.Behavior
import akka.stream.scaladsl.FileIO
//import akka.http.scaladsl.model.headers.{RequestClientCertificate, `Tls-Session-Info`}

/**
 * LDPR Actor extended with Access Control
 */
object Resource {
  import HttpMethods._
  import StatusCodes._
  //log.info(s"created LDPR($ldprUri,$path)")

  //guard could be a pool?
  //var guard: ActorRef = _

//  @throws[Exception](classOf[Exception])
//  def preStart() = {
//    log.info(s"starting guard for $ldprUri ")
//    guard = context.actorOf(Props(new Guard(ldprUri,List())))
//  }

  def extension(path: FPath) = {
    val file = path.getFileName.toString
    var n = file.lastIndexOf('.')
    if (n>0) file.substring(n+1)
    else ""
  }

  def mediaType(path: FPath): MediaType = MediaTypes.forExtension(extension(path))

  def apply(uri: Uri, file: FPath, name: String): Behavior[RouteHttp] = 
    Behaviors.receive[RouteHttp] { (context, msg) =>

      msg.path match { //todo: deal with POST, PUT, DELETE, ...
        case Nil => msg.replyTo ! HttpResponse(StatusCodes.OK,
          Nil, //todo: no headers for the moment
          HttpEntity.Default(
            ContentType(mediaType(file), () => HttpCharsets.`UTF-8`),
            file.toFile.length(),
            FileIO.fromPath(file)
          )
        )
        case head :: tail => {
          //log.info(s"$this received msg $msg")
          //actually perhaps if one does a PUT on an LDPR one could transform it into a LDPC
          msg.replyTo ! HttpResponse(NotFound)
        }
      }
      Behaviors.stopped
    }
}
