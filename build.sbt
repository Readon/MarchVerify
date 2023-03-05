ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.example"

val spinalVersion = "1.8.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
val orgScalaTest = "org.scalatest" %% "scalatest" % "3.2.5" % "test"
val spinalTester = "com.github.spinalhdl" %% "spinalhdl-tester" % spinalVersion % "test"

lazy val all = (project in file("."))
  .settings(
    name := "WaveformGenerator",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin, orgScalaTest, spinalTester)
  )

fork := true

// (testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-oF")
