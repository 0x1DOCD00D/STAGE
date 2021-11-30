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
val nscalatimeVersion = "2.30.0"
val apacheCommonMathVersion = "3.6.1"
val asmVersion = "9.2"
val guavaVersion = "30.1.1-jre"
val akkaVersion = "2.6.13"
val catsVersion = "2.6.1"
val snakeYamlVersion = "2.3"
val scalaZVersion = "7.4.0-M8"

resolvers += ("Apache Snapshots" at "http://repository.apache.org/content/repositories/snapshots").withAllowInsecureProtocol(true)
resolvers += ("Apache repo" at "https://repository.apache.org/").withAllowInsecureProtocol(true)

//noinspection SpellCheckingInspection
lazy val root = (project in file("."))
  .settings(
    name := "STAGE",
    scalacOptions := Seq("-explain", "-Yexplain-lowlevel", "-Xfatal-warnings", "-unchecked", "-deprecation", "-feature", "-language:implicitConversions"),
    scalacOptions += "-language:experimental.macros",
    description := "Simulation Templatized Agent-based Generation Engine",
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      ("com.typesafe.akka" %% "akka-actor-typed" % akkaVersion).cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test).cross(CrossVersion.for3Use2_13),
      "ch.qos.logback" % "logback-core" % logbackVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "org.slf4j" % "slf4j-api" % sfl4sVersion,
      "com.typesafe" % "config" % typesafeConfigVersion,
      "commons-io" % "commons-io" % apacheCommonIOVersion,
      "org.apache.commons" % "commons-math3" % apacheCommonMathVersion,
      "org.apache.commons" % "commons-rng-simple" % "1.3",
      "org.typelevel" %% "cats-core" % catsVersion,
      "com.github.nscala-time" %% "nscala-time" % nscalatimeVersion,
      "org.scalactic" %% "scalactic" % scalacticVersion,
      "org.scalatest" %% "scalatest" % scalacticVersion % Test,
      "org.scalatest" %% "scalatest-featurespec" % scalacticVersion % Test,
      "org.scalaz" %% "scalaz-core" % scalaZVersion,
      "org.ow2.asm" % "asm" % asmVersion,
      "org.ow2.asm" % "asm-commons" % asmVersion,
      "org.ow2.asm" % "asm-util" % asmVersion,
      "com.google.guava" % "guava" % guavaVersion,
      "com.typesafe" % "config" % typesafeConfigVersion,
      "org.snakeyaml" % "snakeyaml-engine" % snakeYamlVersion,
    ),
    homepage := Some(url("https://github.com/0x1DOCD00D/STAGE")),
    licenses := Seq("STAGE License" -> url("https://github.com/0x1DOCD00D/STAGE/LICENSE")),
    scriptedBufferLog := false,
    publishMavenStyle := false,
  )