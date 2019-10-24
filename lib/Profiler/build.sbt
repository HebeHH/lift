name := "Profiler"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.12.6"
)

val paradiseVersion = "2.1.0"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
