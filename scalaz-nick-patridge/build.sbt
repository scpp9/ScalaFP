name := "scalaz-nick-patridge"

version := "0.1"

scalaVersion := "2.13.6"


libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-kernel" % "2.6.1",

  compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),

  "org.typelevel" %% "cats-laws" % "2.4.2" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.9" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test
)


