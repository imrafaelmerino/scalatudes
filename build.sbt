ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"


lazy val root = (project in file("."))
  .settings(
    name := "scala-bazaar",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test,
    libraryDependencies += "org.scalacheck" % "scalacheck_2.13" % "1.16.0",
  )


