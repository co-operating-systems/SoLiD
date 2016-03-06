package co.os.solid

/**
  * A message to be sent to a specific actor.
  * This is to make it easier to do the routing from root ldpc to
  * actor.
  *
  * It may be that all that is needed is just the paths in the URLs,
  * but those don't have such nice matching features.
  * todo: consider later if removing this class can be done.
  *
  * @param path relative path from the actor to which the message is sent
  * @param message any content that needs to then be pattern matched.
  */
case class Message(path: List[String], message: Any) {
  def next = Message(path.tail,message)
}
