package run.cosy.ldp.fs


class TestAttributesFn extends munit.FunSuite {
	import java.nio.file.{Files, Path}

	val tmpDir = FunFixture[Path](
		setup = { test =>
			Files.createTempDirectory("cosyTest")
		},
		teardown = { dir =>
			// Always gets called, even if test failed.
			import java.nio.file.{Path,FileVisitOption, FileVisitor}
			import java.util.Comparator
			import scala.jdk.StreamConverters.{given,*}
			val files: LazyList[Path] = Files.walk(dir).sorted(Comparator.reverseOrder()).toScala(LazyList)
			files.map(_.toFile).foreach(_.delete)
		}
	)
	
	tmpDir.test("create SymLink") { (dirPath: Path) =>
		println(s"path=$dirPath")
		import java.nio.file.Paths
		import scala.util.Try
		//create symlink
		val att: Try[Attributes.SymLink] = Attributes.createLink(dirPath,"hello","hello.txt")
		assert(att.isSuccess)
		val sl: Attributes.SymLink = att.get
		assertEquals(sl.path,dirPath.resolve("hello"),att)
		assertEquals(sl.att.isSymbolicLink,true,att)
		assertEquals(sl.to,Paths.get("hello.txt"),att)
		
		//teest the created symlink directly
		val slatTry: Try[Attributes.Attributes] = Attributes.forPath(sl.path)
		assert(slatTry.isSuccess)
		val slatt: Attributes.Attributes = slatTry.get
		slatt match {
			case symLink : Attributes.SymLink => 
				import java.nio.file.LinkOption
				assertEquals(symLink.path,dirPath.resolve("hello"))
				assertEquals(symLink.to.toString,"hello.txt",slatt)
				assertEquals(sl.to,symLink.to,slatt)
				assert(Files.exists(symLink.path,LinkOption.NOFOLLOW_LINKS))
				assert(!Files.exists(symLink.path))
				assert(!Files.exists(dirPath.resolve("hello.txt")))
			case _ => assert(false,slatt)	
		}
		
	}
}
