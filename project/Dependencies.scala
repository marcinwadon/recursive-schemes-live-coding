import sbt._

object Dependencies {
  lazy val cats = Seq(
    "org.typelevel" %% "cats-core"
  ).map(_ % "2.1.1")

  lazy val droste = Seq(
    "io.higherkindness" %% "droste-core"
  ).map(_ % "0.8.0")

  lazy val scalaTest = Seq(
    "org.scalatest" %% "scalatest" % "3.2.2"
  ).map(_ % Test)
}
