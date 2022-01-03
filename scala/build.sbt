import scala.collection.Seq

val scala3Version = "3.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "demo",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
).map(_ % circeVersion)
