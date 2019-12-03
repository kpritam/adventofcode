name in ThisBuild := "adventofcode"
organization in ThisBuild := "io.kpritam"
scalaVersion in ThisBuild := "2.13.1"
version in ThisBuild := "0.1.0-SNAPSHOT"

fork in ThisBuild := true

scalacOptions in ThisBuild ++= Seq(
  // see https://docs.scala-lang.org/overviews/compiler-options/index.html#Standard_Settings
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-explaintypes",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-opt-warnings",
  "-opt:l:inline",
  "-opt-inline-from:<source>",
  "-unchecked",
  "-Yrangepos",
  "-Ywarn-extra-implicit",
  "-Ywarn-numeric-widen",
  "-Ywarn-self-implicit",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xsource:2.12"
)

lazy val adventofcode = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.0-M1" % Test
    )
  )
