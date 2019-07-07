lazy val commonSettings = Seq(
  version := "v0.1",
  scalaVersion := "2.12.6",
)

lazy val root = (project in file("."))
    .settings(
      commonSettings,
      name := "root"
    )