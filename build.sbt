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
lazy val specs2Version = "4.6.0"
lazy val cats = (project in file("cats"))
  .settings(
    commonSettings,
    name := "cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.typelevel" %% "cats-laws" % catsVersion,
      "org.typelevel" %% "cats-mtl-core" % "0.2.2",

      "org.typelevel" %% "algebra" % "1.0.1",
      "org.typelevel" %% "algebra-laws" % "1.0.1",

      "com.github.mpilquist" %% "simulacrum" % "0.19.0",

      "org.scalamacros" %% "resetallattrs" % "1.0.0",

      "org.specs2" %% "specs2-core" % specs2Version % Test,
      "org.specs2" %% "specs2-scalacheck" % specs2Version % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-Ypartial-unification",
      "-feature",
      "-language:_"
    )
  )