import xerial.sbt.Sonatype._

// Basic project information
organization := "io.github.pschaus"
name := "oscar-cp"
version := "4.0.0"
scalaVersion := "3.3.8"

// Compiler settings
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
scalacOptions ++= Seq("-source", "3.0-migration", "-no-indent")

// Dependencies
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test        // ScalaTest for unit testing
)

// Javadoc and Sources JAR Generation (Required by Maven Central)
Compile / packageDoc / publishArtifact := true
Compile / packageSrc / publishArtifact := true

// Maven Central / Sonatype Metadata & Settings
description := "OscaR: Operations Research in Scala - Constraint Programming Solver"
homepage := Some(url("https://github.com/pschaus/oscar"))
licenses := Seq("LGPL-3.0" -> url("http://www.gnu.org/licenses/lgpl-3.0.en.html"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/pschaus/oscar"),
    "scm:git@github.com:pschaus/oscar.git"
  )
)

developers := List(
  Developer(
    id    = "pschaus",
    name  = "Pierre Schaus",
    email = "pierre.schaus@uclouvain.be",
    url   = url("http://www.info.ucl.ac.be/~pschaus/")
  )
)

// GPG Passphrase for sbt-pgp
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toCharArray).orElse(Some("oscarlib".toCharArray))

// Publishing settings for Sonatype Central Portal
publishMavenStyle := true
sonatypeCredentialHost := "central.sonatype.com"
sonatypeBundleDirectory := (ThisBuild / baseDirectory).value / "target" / "sonatype-staging" / (version.value + "-bundle")

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    sonatypePublishToBundle.value
}