name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)

mainClass := Some("Main")