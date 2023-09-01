import oscar.OscarBuild
import oscar.OscarBuild._


resolvers += Resolver.typesafeRepo("releases")

lazy val aggregatedProjects: Seq[ProjectReference] = Seq(oscarAlgo,oscarCp)

lazy val root = (project in file(".")) // has to be named root.
  .settings(commonSettings: _*)
  .aggregate(aggregatedProjects: _*)
  .settings(name := "oscar")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(ScalaUnidocPlugin.globalSettings)
  .settings(unidocProjectFilter in(ScalaUnidoc, unidoc) := inAnyProject)



lazy val oscarAlgo = (project in file("oscar-algo"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-algo")
  .settings(libraryDependencies ++= Dependencies.testDeps)



lazy val oscarCp = (project in file("oscar-cp")) // TODO pack : pack auto settings?
  .settings(commonSettings: _*)
  .settings(name := "oscar-cp")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .dependsOn(oscarAlgo)

lazy val oscarCpExample = (project in file("oscar-cp-examples")) // TODO pack : pack auto settings?
  .settings(commonSettings: _*)
  .settings(name := "oscar-cp-examples")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .dependsOn(oscarCp)


