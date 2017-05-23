name in ThisBuild := "derive"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.11"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.2")

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full)

libraryDependencies ++=
  "org.scalameta" %% "scalameta" % "1.7.0" ::
  "org.scalatest" %% "scalatest" %  "3.0.3" % "test" ::
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
  Nil

organization in Global := "com.github.cornerman"

pgpSecretRing in Global := file("secring.gpg")
pgpPublicRing in Global := file("pubring.gpg")
pgpPassphrase in Global := Some("".toCharArray)

pomExtra := {
  <url>https://github.com/cornerman/derive</url>
  <licenses>
    <license>
      <name>The MIT license</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/cornerman/derive</url>
    <connection>scm:git:git@github.com:cornerman/derive.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jkaroff</id>
      <name>Johannes Karoff</name>
      <url>https://github.com/cornerman</url>
    </developer>
  </developers>
}
