name := "derive"
version := "0.1.0-SNAPSHOT"

// scala.meta macros are at the moment only supported in 2.11.
scalaVersion in ThisBuild := "2.11.8"

resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")

addCompilerPlugin( "org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full)

libraryDependencies ++=
  "org.scalameta" %% "scalameta" % "1.4.0" ::
  "org.specs2" %% "specs2-core" % "3.8.4" % "test" ::
  "org.specs2" %% "specs2-mock" % "3.8.4" % "test" ::
  Nil

// temporary workaround for https://github.com/scalameta/paradise/issues/10
scalacOptions in (Compile, console) := Seq() // macroparadise plugin doesn't work in repl yet.
// temporary workaround for https://github.com/scalameta/paradise/issues/55
sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  "-Xplugin-require:macroparadise" ::
  Nil
