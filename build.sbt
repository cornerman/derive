name := "derive"
version := "0.0.1"

scalaVersion := "2.11.8"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++=
  "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
  "com.github.cornerman" %% "macroni" % "0.0.1-SNAPSHOT" % "test" ::
  "org.specs2" %% "specs2-core" % "3.8.4" % "test" ::
  "org.specs2" %% "specs2-mock" % "3.8.4" % "test" ::
  Nil

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  Nil
