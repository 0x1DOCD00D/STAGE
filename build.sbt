ThisBuild / organization := "com.lsc"
ThisBuild / version := {
  val orig = (ThisBuild / version).value
  if (orig.endsWith("-SNAPSHOT")) "1.0.A-SNAPSHOT"
  else orig
}
ThisBuild / scalaVersion := "3.0.2"

val logbackVersion = "1.3.0-alpha10"
val sfl4sVersion = "2.0.0-alpha5"
val typesafeConfigVersion = "1.4.1"
val apacheCommonIOVersion = "2.11.0"
val scalacticVersion = "3.2.9"
val cloudSimPlusVersion = "6.4.3"


lazy val root = (project in file("."))
  .settings(
    name := "STAGE",
    scalacOptions := Seq("-explain", "-Yexplain-lowlevel", "-Xfatal-warnings", "-unchecked", "-deprecation", "-feature", "-language:implicitConversions"),
    scalacOptions += "-language:experimental.macros",
    description := "Simulation Templatized Agent-based Generation Engine",
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-core" % logbackVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "org.slf4j" % "slf4j-api" % sfl4sVersion,
      "com.typesafe" % "config" % typesafeConfigVersion,
      "commons-io" % "commons-io" % apacheCommonIOVersion,
      "org.scalactic" %% "scalactic" % scalacticVersion,
      "org.scalatest" %% "scalatest" % scalacticVersion % Test,
      "org.scalatest" %% "scalatest-featurespec" % scalacticVersion % Test,
      "com.typesafe" % "config" % typesafeConfigVersion,
      "org.cloudsimplus" % "cloudsim-plus" % cloudSimPlusVersion
    ),
    homepage := Some(url("https://github.com/0x1DOCD00D/STAGE")),
    licenses := Seq("STAGE License" -> url("https://github.com/0x1DOCD00D/STAGE/LICENSE")),
    scriptedBufferLog := false,
    publishMavenStyle := false,
  )