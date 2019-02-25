scalaVersion := "2.12.8"

scalacOptions := Seq("-deprecation",
                     "-Xfuture",
                     "-Ywarn-unused",
                     "-Ywarn-unused-import",
                     "-Ywarn-dead-code",
                     "-Ywarn-numeric-widen")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

mainClass in (Compile, packageBin) := Some("xyz.minond.bisquit.Main")
mainClass in (Compile, run) := Some("xyz.minond.bisquit.Main")
