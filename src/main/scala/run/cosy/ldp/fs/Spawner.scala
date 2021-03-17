package run.cosy.ldp.fs

import akka.actor.typed.scaladsl.ActorContext
import akka.http.scaladsl.model.Uri
import run.cosy.ldp.ResourceRegistry
import Attributes.{DirAtt, SymLink}
import BasicContainer.{ChildTerminated, Cmd}

/**
 * Enforce a limited actor spawn behavior that can be tested easily
 * for a Container.
 * As a Value Type to avoid having to create an object  
 **/
class Spawner(val context: ActorContext[Cmd]) extends AnyVal {

	import org.slf4j.Logger
	import run.cosy.ldp.fs.Attributes.ActorFileAttr

	def spawnDir(dir: DirAtt, url: Uri)(
		using reg: ResourceRegistry
	): CRef = {
		val name = dir.path.getFileName.toString
		val ref = context.spawn(BasicContainer(url, dir.path), name)
		context.watchWith(ref, ChildTerminated(name))
		CRef(dir, ref)
	}

	def spawnSymLink(link: SymLink, url: Uri): RRef = {
		val name = link.path.getFileName.toString
		val ref = context.spawn(Resource(url, link.path, link.to, name), name)
		context.watchWith(ref, ChildTerminated(name))
		RRef(link, ref)
	}

	def spawn(dir: ActorFileAttr, url: Uri)(
		using reg: ResourceRegistry
	): Ref =
		import org.slf4j.Logger
		dir match 
		case d: DirAtt => spawnDir(d,url)
		case s: SymLink => spawnSymLink(s,url)
	
	
	def log: Logger = context.log
}