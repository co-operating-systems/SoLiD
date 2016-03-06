package co.os

import java.io.{File, FileInputStream}
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
import co.os.solid.{Message, LDPCActor}
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

