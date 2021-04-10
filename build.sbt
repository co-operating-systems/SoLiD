import Dependencies._

val Scala3Version = "3.0.0-RC2"

lazy val root = project
	.in(file("."))
	.settings(
		name := "cosy",
		description := "Reactive Solid",
		version := "0.1.2",
		scalaVersion := Scala3Version,

		//resolvers += Resolver.bintrayRepo("akka","snapshots"), //use if testing akka snapshots
		resolvers += Resolver.sonatypeRepo("snapshots"), //for banana-rdf

		libraryDependencies ++= dottyCompatLibs,
		libraryDependencies ++= javaLibs,
		libraryDependencies ++= scala3Libs,

		testFrameworks += new TestFramework("munit.Framework")
	)
