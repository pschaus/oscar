// Basic project information
organization := "org.oscarcp"
name := "org.oscarcp"
version := "1.0.0-SNAPSHOT"
scalaVersion := "3.3.8"

// Compiler settings
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
scalacOptions ++= Seq("-source", "3.0-migration", "-rewrite")

// Dependencies
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test        // ScalaTest for unit testing
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