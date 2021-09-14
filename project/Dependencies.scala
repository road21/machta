import sbt._

object Dependencies {
  object Version {
    val scala3 = "3.0.2"
    val cats = "2.6.1"
    val circe = "0.14.1"
    val scalaTest = "3.2.9"
  }

  lazy val catsCore = "org.typelevel" %% "cats-core" % Version.cats
  lazy val circeCore = "io.circe" %% "circe-core" % Version.circe
  lazy val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest
}

