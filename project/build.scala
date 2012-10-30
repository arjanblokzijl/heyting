import sbt._
import Keys._


object HeytingBuild extends Build {
  import Dependencies._
  lazy val root = Project(
    id = "heyting",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(compiler)
  )

  lazy val compiler = Project(
    id = "compiler",
    base = file("compiler"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Specs)
    )
  )

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.ab",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.0-RC1",
    crossPaths := false,
    scalacOptions  ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-language:_"),
    resolvers ++= Seq("releases" at "http://oss.sonatype.org/content/repositories/releases",
                        "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")
  )
}

object Dependencies {
//  def ScalaCheck = "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"

  def Specs = "org.specs2" % "specs2_2.10.0-RC1" % "1.13-SNAPSHOT" % "test"
}