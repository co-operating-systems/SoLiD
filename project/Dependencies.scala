import sbt._

/**
 * https://www.scala-sbt.org/1.x/docs/Organizing-Build.html
 */
object Dependencies {
	val AkkaVersion = "2.6.13"
	val AkkaHttpVersion = "10.2.4"
	val scalazVersion = "7.4.0-M7"
	val circeVersion = "0.14.0-M4"
	val bananaVersion = "0.9.0-SNAPSHOT"
	val alpakkaVersion = "2.0.2"

	type NeedsDottyCompat = List[ModuleID]
	val NeedsDottyCompat = List

	//
	// scala 2.13 libs
	//

	/**
	 * Akka Http Core
	 * Apache 2 License
	 * @see https://akka.io
	 * @see https://repo1.maven.org/maven2/com/typesafe/akka
	 * */
	val akka = NeedsDottyCompat("com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
		"com.typesafe.akka" %% "akka-stream" % AkkaVersion,
		"com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
		"com.typesafe.akka" %% "akka-slf4j" % AkkaVersion)

	/**
	 * Apache 2 License
	 * @see https://doc.akka.io/docs/alpakka/current/
	 */
	val alpakka = "com.lightbend.akka" %% "akka-stream-alpakka-file" % alpakkaVersion


	val akkaTest = NeedsDottyCompat("com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
		"com.typesafe.akka" %% "akka-stream-testkit" % AkkaVersion % Test,
		"com.typesafe.akka" %% "akka-http-testkit" % AkkaHttpVersion % Test)

	/**
	 * banana-rdf uses Scalaz so we won't use cats right now.
	 * There is a 3.0.0 version of scalaz out, but until banana-rdf compiles to 3.0.0 we
	 * need to use the 2.13 scala version
	 * @see https://scalaz.github.io/7/
	 * @see [[https://github.com/scalaz/scalaz/blob/master/LICENSE.txt License]]
	 */
	val scalaz  = "org.scalaz" %% "scalaz-core" %  scalazVersion

	/**
	 * banana-rdf is still using 2.13
	 * [[https://github.com/banana-rdf/banana-rdf/blob/series/0.8.x/LICENSE.md W3C License]]
	 * @see https://github.com/banana-rdf/banana-rdf
	 */
	val banana = NeedsDottyCompat(
		"net.bblfish.rdf" %% "banana-rdf" % bananaVersion,
		"net.bblfish.rdf" %% "banana-jena" % bananaVersion
	)

	def dottyCompatLibs: NeedsDottyCompat = akka ++ akkaTest ++ banana ++ Seq(scalaz, alpakka)

	//
	// Scala 3 libs
	//

	/**
	 * [[https://www.scalatest.org/install home page]] published under Apache 2 License
	 * @see [[https://mvnrepository.com/artifact/org.scalatest/scalatest maven]]
	 * */
	val scalatest = "org.scalatest" %% "scalatest" % "3.2.7" % Test

	/**
	 * Apache 2 License
	 * @see [[https://scalameta.org/munit/docs/getting-started.html Getting Started]]
	 * @see https://mvnrepository.com/artifact/org.scalameta/munit
	 */
	val munit = "org.scalameta" %% "munit" % "0.7.23" % Test

	val scala3Libs = Seq(scalatest, munit)
	//
	// Java Libs
	//

	/**
	 * Apache 2 License
	 * Tomitribe HTTP Signatures implementation in Java
	 * @see https://github.com/tomitribe/http-signatures-java
	 */
	val tomitribeHttpSig = "org.tomitribe" % "tomitribe-http-signatures" % "1.7"

	/**
	 * Apache 2 License
	 * Titanium JSON-LD 1.1 parser. Only Java parser able to parse security vocab files it seems.
	 * Should be integrated into banana-rdf.
	 * @see https://github.com/filip26/titanium-json-ld
	 */
	val titaniumJSonLD = "com.apicatalog" % "titanium-json-ld" % "1.0.0"

	/**
	 * Apache 2 License
	 * @see  https://connect2id.com/products/nimbus-jose-jwt/examples/jwk-conversion
	 */
	val nimbusDS = "com.nimbusds" % "nimbus-jose-jwt" % "9.7"

	/**
	 * License [[http://logback.qos.ch/license.html EPL v1.0 and the LGPL 2.1]]
	 *  used by Akka logging
	 *  @see http://logback.qos.ch/download.html
	 */
	val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

	/**
	 * Apache 2.0
	 * @see [[https://mvnrepository.com/artifact/commons-codec/commons-codec Apache Commons Codec]]
	 */
	val apacheCommonsCodec = "commons-codec" % "commons-codec" % "1.15"

	val javaLibs = Seq(tomitribeHttpSig, titaniumJSonLD, nimbusDS, logback, apacheCommonsCodec)


}

//libraries that could be used later

// https://circe.github.io/circe/
//		libraryDependencies ++= Seq(
//			"io.circe" %% "circe-core",
//			"io.circe" %% "circe-generic",
//			"io.circe" %% "circe-parser"
//		).map(_ % circeVersion),

//	// https://connect2id.com/products/nimbus-jose-jwt/examples/jwk-conversion
//			"com.nimbusds" % "nimbus-jose-jwt" % "9.7",
//			"org.glassfish" % "jakarta.json" % "2.0.0",

////https://mvnrepository.com/artifact/org.typelevel/cats-core
//				"org.typelevel" %% "cats-core" % "2.5.0"

//https://mvnrepository.com/artifact/com.lihaoyi/utest
//https://github.com/lihaoyi/utest
//libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % "test",
// testFrameworks += new TestFramework("utest.runner.Framework")