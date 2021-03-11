package run.cosy.ldp.fs

import akka.actor.testkit.typed.CapturedLogEvent
import akka.actor.testkit.typed.Effect._
import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, TestInbox}
import akka.actor.typed
import akka.actor.typed.scaladsl
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.event.Level


class TestContainerSpec extends AnyWordSpec with BeforeAndAfterAll with Matchers {
	import akka.http.scaladsl.model.Uri

	import java.nio.file
	val testKit = ActorTestKit()

	"Start Root Container" must {
		import akka.actor.typed.Behavior
		import akka.http.scaladsl.model.Uri.Path
		import run.cosy.ldp.ResourceRegistry
		import run.cosy.ldp.fs.BasicContainer

		import java.nio.file.Files

		val rootUri = Uri("http://localhost:8080/")
		val rootPath = file.Paths.get("test")
		
		given registry: ResourceRegistry = ResourceRegistry(testKit.system)
		val rootCntr: Behavior[BasicContainer.Cmd] = BasicContainer(rootUri,rootPath)
		
		"record spawning" in {
			 rootCntr.
		}
	}

	override def afterAll(): Unit = testKit.shutdownTestKit()
}
