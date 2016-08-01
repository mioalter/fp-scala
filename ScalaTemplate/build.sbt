name := "scala-template"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
  , "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test"
  , "com.github.nscala-time" %% "nscala-time" % "2.12.0"
)

mainClass in (Compile, run) := Some("scalatemplate.Main")