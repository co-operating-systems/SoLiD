package co.os.solid

import java.security.Principal
import java.nio.file.{Files, Path => FPath}

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{RequestClientCertificate, `Tls-Session-Info`}

/**
  * LDPR Actor extended with Access Control
  */
class LDPRActor(ldprUri: Uri, path: FPath) extends BaseActor {
  import HttpMethods._
  import StatusCodes._
  println(s"created LDPR($ldprUri,$path)")

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
      case head::tail =>
        println(s"$this received msg $msg")
        context.sender !  HttpResponse(NotFound)
    }
    case req@HttpRequest(GET,uri,headers,entity,protocol) => {
       context.sender ! {
         if (ldprUri.path == uri.path) {
           def responseForPrincipal(principal: Principal): HttpResponse =
             HttpResponse(entity = s"Hello ${principal.getName}!")

           req.header[`Tls-Session-Info`] match {
             case Some(infoHeader) if infoHeader.peerPrincipal.isDefined ⇒
               responseForPrincipal(infoHeader.peerPrincipal.get)
//               HttpResponse(OK,
//                 entity = HttpEntity(
//                   //todo: encode mime type in file name or somewhere
//                   contentType=ContentType(mediaType(path),()=>HttpCharsets.`UTF-8`),
//                   path.toFile
//                 )
//               )
             case _ ⇒
               println("requesting client certificate!!!")
               HttpResponse(headers = RequestClientCertificate(req) :: Nil)
           }
         } else HttpResponse(NotFound, entity = s"could not find $req")
       }
    }
    case other =>  context.sender ! HttpResponse(BadRequest, entity=s"Bad request $other")
  }

}
