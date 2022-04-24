name := "numnum"

version := "0.0.1"

scalaVersion := "2.13.5"

lazy val root = project
  .in(file(".")).settings(
    libraryDependencies ++= Seq(
      "com.chuusai"   %% "shapeless"         % "2.3.9",
      "org.scala-lang" % "scala-reflect"     % scalaVersion.value,
      "org.scalatest" %% "scalatest-funspec" % "3.2.11" % "test"
    )
  )

lazy val nat = project.in(file("nat"))
