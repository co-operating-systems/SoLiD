package co.os.solid

import java.security.PublicKey
import java.security.cert.X509Certificate

import akka.http.javadsl.model.ResponseEntity
import akka.http.scaladsl.model
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers.{RequestClientCertificate, `Tls-Session-Info`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, Uri}

/**
  * A guard for a resource.
  *
  * to test: if file ends with .scala end user has WebID then he gets access
  * otherwise he gets read access
  */
class Guard(forResource: Uri, acls: List[Uri]) extends BaseActor {
  import Guard._

  log.info(s"created Guard for $forResource")

  override
  def receive = returnErrors {
    case req @ HttpRequest(GET,uri,headers,_,_) => {
      log.info(s"guard received $req")
      if (uri.path.reverse.head.toString.endsWith(".scala")) {
        log.info("guard: considering acl for .scala file")
        req.header[`Tls-Session-Info`] match {
          case Some(infoHeader) if !infoHeader.peerCertificates.isEmpty ⇒ {
            val ids = infoHeader.peerCertificates.headOption.collect { case x: X509Certificate => x }
            ids match {
              case Some(cert) =>
                if (extractWebIds(cert).size > 0)
                  context.parent forward Authorized(req)
                else {
                  sender ! HttpResponse(Unauthorized,entity = "no WebID")
                }
              case None => sender ! HttpResponse(Unauthorized,entity="no Certs")
            }
          }
          case _ if !req.headers.contains(XCert)  ⇒ {
            log.info("requesting client certificate!!!")
            sender ! HttpResponse(headers = RequestClientCertificate(req.addHeader(XCert)) :: Nil)
          }
          case _ => sender ! HttpResponse(Unauthorized,
            entity="The client certificate was requested but not found")
        }
      } else {
        context.parent forward Authorized(req)
      }
    }
    case _: HttpRequest =>
      sender ! HttpResponse(Unauthorized)
  }
}

case class Authorized(httpreq: HttpRequest)


object Guard {

  val XCert = HttpHeader.parse("X-CertRequested","1") match {
    case Ok(h,e) => h
  }


  def extractWebIds(x509: X509Certificate): List[(String, PublicKey)] =
      Option(x509.getSubjectAlternativeNames).toList.flatMap { collOfNames =>
        import scala.collection.JavaConverters.iterableAsScalaIterableConverter
        for {
          sanPair <- collOfNames.asScala
          if (sanPair.get(0) == 6)
        } yield (sanPair.get(1).asInstanceOf[String].trim,x509.getPublicKey)
      }

}
