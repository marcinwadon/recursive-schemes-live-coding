import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "net.wadon"
ThisBuild / organizationName := "wadon"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

lazy val root = (project in file("."))
  .settings(
    name := "recursion-schemes",
    libraryDependencies ++= scalaTest ++ cats ++ droste
  )

