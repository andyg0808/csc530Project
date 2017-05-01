import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "work.k33.calpoly.csc530",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "PACT Tester",
    libraryDependencies += scalaTest % Test
  )
