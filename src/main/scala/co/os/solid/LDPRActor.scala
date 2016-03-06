package co.os.solid

import java.nio.file.{Path => FPath}
import java.security.Principal

import akka.actor.{Props, ActorRef}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{RequestClientCertificate, `Tls-Session-Info`}

/**
  * LDPR Actor extended with Access Control
  */
class LDPRActor(ldprUri: Uri, path: FPath) extends BaseActor {
  import HttpMethods._
  import StatusCodes._
  log.info(s"created LDPR($ldprUri,$path)")

  //guard could be a pool?
  var guard: ActorRef = _

  @throws[Exception](classOf[Exception])
  override
  def preStart() = {
     log.info(s"starting guard for $ldprUri ")
     guard = context.actorOf(Props(new Guard(ldprUri,List())))
  }

  def extension(path: FPath) = {
    val file = path.getFileName.toString
    var n = file.lastIndexOf('.')
    if (n>0) file.substring(n+1)
    else ""
  }

  def mediaType(path: FPath) = MediaTypes.forExtension(extension(path))

  override
  def receive = returnErrors {
    case msg: Message => msg.path match {
      case Nil => this.receive(msg.message)
      case head::tail => {
        log.info(s"$this received msg $msg")
        //actually perhaps if one does a PUT on an LDPR one could transform it into a LDPC
        context.sender ! HttpResponse(NotFound)
      }
    }
    case req@HttpRequest(GET,uri,headers,entity,protocol) =>
      guard forward req
    case other: HttpRequest => sender ! HttpResponse(MethodNotAllowed)
    case Authorized(req) =>
       context.sender ! HttpResponse(OK,
         entity = HttpEntity(
           //todo: encode mime type in file name or somewhere
           contentType = ContentType(mediaType(path), () => HttpCharsets.`UTF-8`),
           path.toFile
         )
       )
    case other =>  context.sender ! HttpResponse(BadRequest, entity=s"Bad request $other")
  }

}
