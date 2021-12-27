ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.9"

lazy val root = (project in file("."))
  .settings(
    name := "sampleaudio"
  )
