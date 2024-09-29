// Basic project information
organization := "com.yourcompany"
name := "oscar"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.13.11"

// Compiler settings
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// Dependencies
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "junit" % "junit" % "4.12" % Test,                    // JUnit for unit testing
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,    // ScalaCheck for property-based testing
  "org.scalatest" %% "scalatest" % "3.2.16" % Test        // ScalaTest for standard unit tests
)

// Repository to deploy artifacts (e.g., GitHub Packages)
publishTo := Some("GitHub Packages" at "https://maven.pkg.github.com/pschaus/oscar")

// Add credentials for GitHub (make sure you have a valid credentials file)
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")


// Javadoc and Sources JAR Generation
Compile / packageDoc / publishArtifact := true     // Enable Javadoc JAR generation
Compile / packageSrc / publishArtifact := true     // Enable Source JAR generation


// Repository resolver for additional libraries
resolvers += "GitHub" at "https://maven.pkg.github.com/pschaus/oscar"