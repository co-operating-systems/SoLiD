package co.os.solid

import java.nio.file.{Files, Path => FPath}


import akka.actor.{Actor, Props}
import akka.http.scaladsl.model._

/**
  * LDPC Actor extended with Web Access Control and other SoLiD features.
  */
class LDPCActor(ldpcUri: Uri, root: FPath) extends LDPRActor(ldpcUri,root) {
  import HttpMethods._
  import StatusCodes._
  log.info(s"created LDPC($ldpcUri,$root)")

  override
  def receive = returnErrors {
    case msg: Message => msg.path match {
      case Nil => this.receive(msg.message)
      case head :: tail =>
        context.child(head) match {
          case Some(ref) => ref forward msg.next
          case None => msg.message match {
            case HttpRequest(PUT, _, _, _,_) => {
              //special case because it could be requesting to make intermediary subdirectories
              context.sender ! HttpResponse(MethodNotAllowed)
            }
            case other => {
              val file = root.resolve(head)
              log.info(s"${msg.message} has checking file=$file")
              if (Files.exists(file)) {
                val newref = if (Files.isDirectory(file)) {
                  context.actorOf(Props(new LDPCActor(ldpcUri.withPath(ldpcUri.path/head), file)), head)
                } else {
                  context.actorOf(Props(new LDPRActor(ldpcUri.withPath(ldpcUri.path/head), file)), head)
                }
                newref forward msg.next
              } else {
                context.sender ! HttpResponse(NotFound)
              }
            }
          }
        }
    }
    case req@HttpRequest(method,uri,headers,entity,protocol) =>
      context.sender ! {
        if (uri.path == ldpcUri.path)
          HttpResponse(OK, entity = s"Container received $req")
        else {
          HttpResponse(NotFound, entity= s"could not find $uri")
        }
      }
  }
}


