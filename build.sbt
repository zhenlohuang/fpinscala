val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
