
ThisBuild / organization := "jeremy"
ThisBuild / scalaVersion := "2.12.12"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "jeremy",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % Test
    )
  )