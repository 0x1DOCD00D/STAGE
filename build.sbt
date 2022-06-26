
ThisBuild / organization := "com.lsc"
ThisBuild / version := {
  val orig = (ThisBuild / version).value
  if (orig.endsWith("-SNAPSHOT")) "1.0.A-SNAPSHOT"
  else orig
}
ThisBuild / scalaVersion := "3.1.1"

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case x => MergeStrategy.first
}

val logbackVersion = "1.2.10"
val sfl4sVersion = "2.0.0-alpha5"
val typesafeConfigVersion = "1.4.1"
val apacheCommonIOVersion = "2.11.0"
val scalacticVersion = "3.2.9"
val nscalatimeVersion = "2.30.0"
val apacheCommonMathVersion = "3.6.1"
val asmVersion = "9.2"
val guavaVersion = "31.1-jre"
val akkaVersion = "2.6.18"
val catsVersion = "2.7.0"
val catsEffectVersion = "3.4-389-3862cf0"
val snakeYamlVersion = "2.3"
val scalaZVersion = "7.4.0-M8"
val scalaCompilerVersion = "2.13.8"
val scalaReflectVersion = "2.13.8"
val typesafeScalaLogging = "3.9.4"
val protobufVersion = "1.0.1"
val catsScalatestEffects = "1.4.0"

resolvers += ("Apache Snapshots" at "http://repository.apache.org/content/repositories/snapshots").withAllowInsecureProtocol(true)
resolvers += ("Apache repo" at "https://repository.apache.org/").withAllowInsecureProtocol(true)

lazy val root = (project in file("."))
  .settings(
    name := "STAGE",
    scalacOptions := Seq("-encoding", "utf-8", "-Xfatal-warnings", "-unchecked", "-deprecation", "-feature", "-language:implicitConversions"),
    description := "Simulation Templatized Agent-based Generation Engine",
    run / cinnamon := true,
    test / cinnamon := true,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % typesafeScalaLogging,
      "com.typesafe" % "config" % typesafeConfigVersion,
      "commons-io" % "commons-io" % apacheCommonIOVersion,
      "org.apache.commons" % "commons-math3" % apacheCommonMathVersion,
      "org.apache.commons" % "commons-rng-simple" % "1.3",
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-laws" % catsVersion % Test,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "com.github.nscala-time" %% "nscala-time" % nscalatimeVersion,
      "org.scalactic" %% "scalactic" % scalacticVersion,
      "org.scalatest" %% "scalatest" % scalacticVersion % Test,
      "org.scalatest" %% "scalatest-featurespec" % scalacticVersion % Test,
      "org.scalaz" %% "scalaz-core" % scalaZVersion,
      "org.ow2.asm" % "asm" % asmVersion,
      "org.ow2.asm" % "asm-commons" % asmVersion,
      "org.ow2.asm" % "asm-util" % asmVersion,
      "com.google.guava" % "guava" % guavaVersion,
      "com.github.os72" % "protobuf-dynamic" % protobufVersion,
      "com.thesamet.scalapb" %% "scalapb-runtime" % "0.11.8" % "protobuf",
      "org.snakeyaml" % "snakeyaml-engine" % snakeYamlVersion,
      "org.typelevel" %% "cats-effect-testing-scalatest" % catsScalatestEffects % Test
),
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    homepage := Some(url("https://github.com/0x1DOCD00D/STAGE")),
    licenses := Seq("STAGE License" -> url("https://github.com/0x1DOCD00D/STAGE/LICENSE")),
    publishMavenStyle := false,
    Global / onChangedBuildSource := IgnoreSourceChanges
  ).enablePlugins(Cinnamon) aggregate runtimePlatform dependsOn runtimePlatform

lazy val runtimePlatform = (project in file("RuntimePlatform"))
  .settings(
    name := "StageRuntime",
    scalaVersion := "2.13.8",
    scalacOptions := Seq("-encoding", "utf-8", "-Xfatal-warnings", "-unchecked", "-deprecation", "-feature"),
    description := "Stage Actor Execution Platform",
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
      "org.typelevel" %% "cats-effect-testing-scalatest" % catsScalatestEffects % Test,
      // Use Coda Hale Metrics and Akka instrumentation
      Cinnamon.library.cinnamonCHMetrics,
      Cinnamon.library.cinnamonJvmMetricsProducer,
      Cinnamon.library.cinnamonPrometheus,
      Cinnamon.library.cinnamonPrometheusHttpServer,
      // Use Akka instrumentation
      Cinnamon.library.cinnamonAkka,
      Cinnamon.library.cinnamonAkkaTyped,
      Cinnamon.library.cinnamonAkkaPersistence,
      Cinnamon.library.cinnamonAkkaStream,
      Cinnamon.library.cinnamonAkkaProjection,
      // Use Akka HTTP instrumentation
      Cinnamon.library.cinnamonAkkaHttp,
      // Use Akka gRPC instrumentation
      Cinnamon.library.cinnamonAkkaGrpc,

      "org.specs2" %% "specs2-core" % "4.13.2" % Test,
      "org.scala-lang" % "scala-reflect" % scalaReflectVersion,
      "org.scala-lang" % "scala-compiler" % scalaCompilerVersion
    ),
    homepage := Some(url("https://github.com/0x1DOCD00D/STAGE")),
    licenses := Seq("STAGE License" -> url("https://github.com/0x1DOCD00D/STAGE/LICENSE")),
    publishMavenStyle := false,
    Global / onChangedBuildSource := IgnoreSourceChanges
  )

javaOptions ++= Seq("-Duser.language=en_US")

Compile / console / scalacOptions := scalacOptions.value

Compile / console / javaOptions := javaOptions.value

fork := true

assembly / assemblyJarName := "Stage_" + (ThisBuild / version).value + ".jar"

Compile / run / packageBin / mainClass := Some("Main")

assembly / mainClass := Some("Main")