val Scala3Version = "3.0.0-RC1"
val AkkaVersion = "2.6.13"
val AkkaHttpVersion = "10.2.4"

/**
 * [[https://www.scalatest.org/install home page]] and [[https://mvnrepository.com/artifact/org.scalatest/scalatest maven]]
 * */
val scalatest = "org.scalatest" %% "scalatest" % "3.2.4" % "test"

lazy val root = project
	.in(file("."))
	.settings(
		name := "cosy",
		description := "Reactive Solid",
		version := "0.1.0",
		useScala3doc := true,

		scalaVersion := Scala3Version,

		resolvers += Resolver.bintrayRepo("akka", "snapshots"),

		libraryDependencies ++= Seq(
			"com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
			"com.typesafe.akka" %% "akka-stream" % AkkaVersion,
			"com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
			"com.typesafe.akka" %% "akka-slf4j" % AkkaVersion,
			// https://doc.akka.io/docs/alpakka/snapshot/
			"com.lightbend.akka" %% "akka-stream-alpakka-file" % "2.0.2+85-7d4f34b3", //"2.0.2",
			//http://logback.qos.ch/download.html
			"ch.qos.logback" % "logback-classic" % "1.2.3",
			"org.typelevel" %% "cats-core" % "2.4.2",
			"org.typelevel" %% "cats-free" % "2.4.2",
			//"com.novocode" % "junit-interface" % "0.11" % "test"
			//"org.scalatest"     %% "scalatest"        % "3.2.2" % "test",
		).map(_.withDottyCompat(scalaVersion.value)),

		libraryDependencies ++= Seq(
			//https://mvnrepository.com/artifact/org.typelevel/cats-core
			"org.scalameta" %% "munit" % "0.7.22" % Test
		),

		// java libs
		libraryDependencies ++= Seq(
			//https://mvnrepository.com/artifact/commons-codec/commons-codec
			"commons-codec" % "commons-codec" % "1.15"
		),
		// https://mvnrepository.com/artifact/org.scalameta/munit
		// https://scalameta.org/munit/docs/getting-started.html
		testFrameworks += new TestFramework("munit.Framework")
		//libraryDependencies += scalatest,

		//https://mvnrepository.com/artifact/com.lihaoyi/utest
		//https://github.com/lihaoyi/utest
		//libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % "test",
		// testFrameworks += new TestFramework("utest.runner.Framework")
	)
