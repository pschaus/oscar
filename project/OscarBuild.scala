package oscar

import sbt.Keys._
import sbt._

object OscarBuild {

  lazy val PerfTest = config("perf") extend (Test)

  lazy val buildSettings = Seq(
    organization := "oscar",
    version := "4.1.0-SNAPSHOT",
    scalaVersion := "2.13.11",
    sbtVersion := "1.6.2"
  )

  lazy val commonSettings = buildSettings ++ Defaults.coreDefaultSettings ++ Seq(
    Compile / scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature",
      "-unchecked", "-language:implicitConversions", "-language:postfixOps", "-opt-warnings"),
    licenses += ("LGPL-3.0", url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    Test / testOptions += ((Test / target) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u", "<%s>" format (t / "streams/test"))
    }).value,
    Test / parallelExecution := false,
    Test / fork := true,
    javacOptions ++= Seq("-encoding", "UTF-8"),
    Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "examples",
    PerfTest / testOptions += ((PerfTest / target) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u", "<%s>" format (t / "streams/test"))
    }).value,
    PerfTest / fork := true,
    PerfTest / parallelExecution := false
  ) ++ (if (!OscarBuildParameters.debug) Seq(Compile / scalacOptions ++= Seq("-Xdisable-assertions", "-opt:l:inline", "-opt-inline-from:oscar.**"))
  else Seq())





  object Resolvers {

  }

  object Dependencies {
    // Regular libraries

    // Test libraries
    val junit = "junit" % "junit" % "4.12" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "3.1.0" % Test

    val junit2 = "junit" % "junit" % "4.12" % PerfTest
    val scalaCheck2 = "org.scalacheck" %% "scalacheck" % "1.14.0" % PerfTest
    val scalaTest2 = "org.scalatest" %% "scalatest" % "3.2.0-M2" % PerfTest

    val testDeps = Seq(junit, scalaCheck, scalaTest)
  }
}
