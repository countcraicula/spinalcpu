ThisBuild / version      := "1.0"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "org.example"

val spinalVersion = "1.7.0"
val spinalCore    = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib     = "com.github.spinalhdl" %% "spinalhdl-lib"  % spinalVersion
val spinalIdslPlugin = compilerPlugin(
  "com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion
)

lazy val cpu = (project in file("."))
  .settings(
    name := "CPU",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )

fork := true
