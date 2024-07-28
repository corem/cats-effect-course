val scala3Version = "3.3.3"
val catsEffectVersion = "3.5.4"

lazy val root = (project in file("."))
  .settings(
    name := "cats-effect-course",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    )
  )
