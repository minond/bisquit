scalaVersion := "2.12.8"

scalacOptions := Seq("-deprecation",
                     "-Ywarn-unused",
                     "-Ywarn-unused-import",
                     "-Ywarn-dead-code",
                     "-Ywarn-numeric-widen")

mainClass in (Compile, packageBin) := Some("xyz.minond.bisquit.Main")
mainClass in (Compile, run) := Some("xyz.minond.bisquit.Main")
