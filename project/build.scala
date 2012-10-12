import sbt._
import Keys._


object HeytingBuild extends Build {

  lazy val root = Project(
    id = "heyting",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(compiler)
  )

  lazy val compiler = Project(
    id = "compiler",
    base = file("compiler"),
    settings = standardSettings
  )

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.ab",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.0-M7",
    crossPaths := false,
    scalacOptions  ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-language:_"),
    resolvers ++= Seq("releases" at "http://oss.sonatype.org/content/repositories/releases",
                        "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")
  )
}
