// Basic project information
organization := "com.yourcompany"
name := "oscar"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.13.11"

// Compiler settings
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// Dependencies
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value
)

// Repository to deploy artifacts (e.g., GitHub Packages)
publishTo := Some("GitHub Packages" at "https://maven.pkg.github.com/pschaus/oscar")

// Add credentials for GitHub (make sure you have a valid credentials file)
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")


// Javadoc jar generation
autoAPIMappings := true
packageDoc in Compile := target.value / "api-docs" / "javadoc.jar"

// Repository resolver for additional libraries
resolvers += "GitHub" at "https://maven.pkg.github.com/pschaus/oscar"