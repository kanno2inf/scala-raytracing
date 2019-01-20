import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "hobione"
ThisBuild / organizationName := "hobione"

lazy val root = (project in file("."))
  .settings(
    name := "scala-raytracing",
    libraryDependencies ++= Seq(processing)
  )
