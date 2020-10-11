name := "evo-scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

val catsVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
)
