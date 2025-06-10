val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "music-player",
    version := sys.env.getOrElse("VERSION", "0.1.0-SNAPSHOT"),
    scalaVersion := scala3Version,

    Docker / packageName := "ventus218/domoticasw-music-player",
    dockerUpdateLatest := true
  )
