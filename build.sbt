val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := Dependencies.Version.scala3,
  libraryDependencies ++= Seq(Dependencies.scalaTest).map(_ % Test)
)

lazy val circe =
  (project in file("modules/circe"))
  .settings(commonSettings)
  .settings(
    name := "machta-circe",
    libraryDependencies ++= Seq(Dependencies.circeCore) ++ Seq(Dependencies.circeParser).map(_ % Test)
  ).dependsOn(core % "compile->compile;test->test")

lazy val core =
  (project in file("modules/core"))
    .settings(commonSettings)
    .settings(
      name := "machta-core",
      libraryDependencies ++= Seq(Dependencies.catsCore)
    )

lazy val machta = project
  .in(file("."))
  .aggregate(
    core,
    circe
  )
