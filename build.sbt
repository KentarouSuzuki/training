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
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.typelevel" %% "cats-mtl-core" % "0.2.1",
      "com.github.mpilquist" %% "simulacrum" % "0.19.0",
      "org.typelevel" %% "cats-mtl-core" % "0.2.1",
      "org.scalamacros" %% "resetallattrs" % "1.0.0",
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
  )