package co.os

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Path => FPath}
import java.security.{Principal, SecureRandom, KeyStore}
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLContext, KeyManagerFactory, X509TrustManager}

import akka.actor._
import akka.http.scaladsl.{HttpsConnectionContext, Http}
import akka.http.scaladsl.model.Uri.Path._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object Solid  {

  val testConf: Config = ConfigFactory.parseString(
    """
    akka.loglevel = INFO
    akka.log-dead-letters = off
    akka.http.server.parsing.tls-session-info-header = on
    """
  )

  def main(args: Array[String]) {
    val path = java.nio.file.Paths.get(args(0))

    if (!Files.exists(path) && !Files.isDirectory(path)) {
      println(s"could not find directory <$path>")
    }

    implicit val system = ActorSystem("my-system",testConf)
    implicit val materializer = ActorMaterializer()
    implicit val ec = system.dispatcher

    val uri = Uri("https://localhost:8443")
    val solid = Solid(uri, Props(new LDPCActor(uri, path)),"SoLiD")

    val bindingFuture = Http().bindAndHandleAsync(
      req => solid.handle(req),
      interface = uri.authority.host.address(),
      port = uri.authority.port,
      connectionContext = servercontext
    )

// low level version:
//    val run = serverSource.to(Sink.foreach { connection =>
//      connection handleWithAsyncHandler (req => solid.handle(req))
//    }).run()

    println(s"Server online at ${uri.toString()}\nPress RETURN to stop...")
    scala.io.StdIn.readLine() // for the future transformations

    bindingFuture.flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete{_ ⇒
      system.terminate()
      solid.terminate()
    } // and shutdown when done

    println("bye!")
  }

  def pathToList(path: Uri.Path): List[String] = path match {
    case Empty => Nil
    case Segment(head, tail) => head :: pathToList(tail)
    case Slash(tail) => "/" :: pathToList(tail)
  }

  def toMessage(req: HttpRequest) = Message(pathToList(req.uri.path).filter(_ != "/"),req)

  def servercontext = {
    val context = SSLContext.getInstance("TLS")
    keyManager("conf/generated.keystore","password").map { kmf=>
      context.init(kmf.getKeyManagers, Array(noCATrustManager), new SecureRandom)
    }.get
    new HttpsConnectionContext(context)
  }

  def keyManager(path: String, password: String) = {
    val keyStore = KeyStore.getInstance(System.getProperty("https.keyStoreType", "JKS"))
//    val password = System.getProperty("https.keyStorePassword", "").toCharArray
    val algorithm = System.getProperty("https.keyStoreAlgorithm", KeyManagerFactory.getDefaultAlgorithm)

    val file = new File(path)
    if (file.isFile) {
      val in = new FileInputStream(file)
      try {
        keyStore.load(in, password.toCharArray)
        val kmf = KeyManagerFactory.getInstance(algorithm)
        kmf.init(keyStore, password.toCharArray)
        Success(kmf)
      } catch {
        case NonFatal(e) => {
          Failure(new Exception("Error loading HTTPS keystore from " + file.getAbsolutePath, e))
        }
      } finally {
        in.close()
      }
    } else {
      Failure(new Exception("Unable to find HTTPS keystore at \"" + file.getAbsolutePath + "\""))
    }
  }
}

object noCATrustManager extends X509TrustManager {
  val nullArray = Array[X509Certificate]()
  def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String) {}
  def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String) {}
  def getAcceptedIssuers() = nullArray
}


case class Solid(baseUri: Uri, ldpc: Props, name: String) {
  import akka.pattern.ask

  import scala.concurrent.duration._
  val systemPath = "co-os"
  val system = ActorSystem(systemPath)
  val rwwActorRef = system.actorOf(ldpc,name)
  implicit val timeout = Timeout(5.second)
  import Solid._


  def handle(request: HttpRequest): Future[HttpResponse] =
    rwwActorRef.ask(toMessage(request)).asInstanceOf[Future[HttpResponse]]

  def terminate() = system.terminate()

}


class LDPCActor(ldpcUri: Uri, root: FPath) extends LDPRActor(ldpcUri,root) {
  import HttpMethods._
  import StatusCodes._
  println(s"created LDPC($ldpcUri,$root)")

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
              println(s"${msg.message} has checking file=$file")
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
      context.sender() ! {
        if (uri.path == ldpcUri.path)
          HttpResponse(OK, entity = s"Container received $req")
        else {
          HttpResponse(NotFound, entity= s"could not find $uri")
        }
      }
  }
}

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

trait BaseActor extends Actor with akka.actor.ActorLogging  {
  /**
    * Permits to catch exceptions and forward them to the sender as a Future failure
    *
    * @param pf
    * @tparam A
    * @tparam B
    * @return
    */
  def returnErrors[A,B](pf: Receive): Receive = new PartialFunction[Any,Unit] {
    //interestingly it seems we can't catch an error here! If we do, we have to return a true or a false
    // and whatever we choose it could have bad sideffects. What happens if the isDefinedAt throws an exception?
    def isDefinedAt(x: Any): Boolean = pf.isDefinedAt(x)
    def apply(a: Any): Unit = try {
      pf.apply(a)
    } catch {
      case e: Exception => sender ! akka.actor.Status.Failure(e)
    }
  }
}

case class Message(path: List[String], message: Any) {
  def next = Message(path.tail,message)
}

//final class FileSystemRoutingLogic(rootActor: ActorRef, rootCtx: ActorContext ) extends RoutingLogic {
//import FileSystemRoutingLogic._
//
//  override def select(message: Any, routees: IndexedSeq[Routee]): Routee = {
//
//      message match {
//        case HttpRequest(_,uri,_,_,_) => {
//          val pathList: List[String] = pathToList(uri.path)
//          def pathToRef(path: List[String], ref: ActorRef): ActorRef = {
//            path match {
//              case head::tail =>rootCtx.child(head) map getOrElse(rootCtx.self)
//              case Nil => rootCtx.self
//            }
//            r
//          }
//          ActorRefRoutee(r.actorSelection(actorPath))
//        }
//        case _ => NoRoutee
//      }
//  }
//}

