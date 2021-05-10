package run.cosy.ldp.fs

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import akka.http.scaladsl.model.Uri
import run.cosy.ldp.fs
import fs.Attributes.{DirAtt, SymLink, ActorFileAttr}
import run.cosy.ldp.ResourceRegistry



//Allows us to distinguish types of resources, without yet having created actors for them.
//later we may want to wrap these in info to tell when the last time we checked the FS was.
/**
 * @tparam name local name of the resource in the directory
 * @tparam att proof of existence. todo: should be wrapped in a time stamped object              
 */
sealed trait Ref

case class CRef (att: DirAtt, actor: ActorRef[BasicContainer.AcceptMsg]) extends Ref

//todo: there are actually two cases SymLinkToNothing, SymLinkToFile and SymLinkToDir
//  the current model is a simplication that has too much implicit
case class RRef (att: SymLink, actor: ActorRef[Resource.AcceptMsg]) extends Ref
