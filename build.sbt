name in ThisBuild := "derive"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.11"

lazy val root = project.in(file(".")).
  aggregate(deriveJS, deriveJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val derive = (crossProject.crossType(CrossType.Pure) in file("."))
  .settings(
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),

    libraryDependencies ++=
      "org.scalameta" %%% "scalameta" % "1.7.0" ::
      "org.scalatest" %%% "scalatest" %  "3.0.3" % "test" ::
      Nil,

    // temporary workaround for https://github.com/scalameta/paradise/issues/10
    scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
    // temporary workaround for https://github.com/scalameta/paradise/issues/55
    sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.

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
  )

lazy val deriveJVM = derive.jvm
lazy val deriveJS = derive.js

organization in Global := "com.github.cornerman"
