package co.os.solid

import akka.actor.Actor

/**
  * Created by hjs on 06/03/2016.
  */
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
