lazy val commonSettings = Seq(
  version := "v0.1",
  scalaVersion := "2.12.6",
)

lazy val root = (project in file("."))
    .settings(
      commonSettings,
      name := "root"
    ).aggregate(
      cats
    )

lazy val catsVersion = "2.0.0-M4"
lazy val cats = (project in file("cats"))
  .settings(
    commonSettings,
    name := "cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion
    )
  )