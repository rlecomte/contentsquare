import sbt._

object Dependencies {
  lazy val tests = Seq(
    "org.scalatest" %% "scalatest" % "3.0.4",
    "org.scalacheck" %% "scalacheck" % "1.13.5"
  ).map(_  % "test")
}
