// Depends on scalatest having published a version with the right version
// supported. See https://repo1.maven.org/maven2/org/scalatest/ and
// https://github.com/scalatest/scalatest/issues/1865.
// val dottyVersion = "0.26.0-RC1"
val dottyVersion = "0.24.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "bisquit",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalaSource in Test := baseDirectory.value / "test",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test,
  )
