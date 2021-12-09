lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2021",
    description := "adventofcode2021 in scala",
    version := "0.1.0",
    scalaVersion := "3.1.0",
    fork := true
  )

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
