scalaVersion := "2.12.8"

scalacOptions := Seq("-deprecation",
                     "-Xfatal-warnings",
                     "-Ywarn-unused",
                     "-Ywarn-unused-import",
                     "-Ywarn-dead-code",
                     "-Ywarn-numeric-widen")

mainClass in (Compile, packageBin) := Some("bisquit.Main")
mainClass in (Compile, run) := Some("bisquit.Main")
