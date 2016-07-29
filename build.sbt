lazy val root = (project in file(".")).settings(
  name := "fpinscala",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.11.8"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
