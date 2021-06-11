package run.cosy.ldp.fs

import akka.actor.typed.ActorRef
import run.cosy.ldp.fs.Attributes.{DirAtt, SymLink, ManagedResource}


sealed trait Ref

case class CRef(att: DirAtt, actor: ActorRef[BasicContainer.AcceptMsg]) extends Ref

//todo: verify list symlinks types
//  SymLinkToSelf (circular), SymLinkToFile and SymLinkToDir
case class RRef(att: SymLink, actor: ActorRef[Resource.AcceptMsg]) extends Ref

case class SMRef(att: ManagedResource, actor: ActorRef[Resource.AcceptMsg]) extends Ref
