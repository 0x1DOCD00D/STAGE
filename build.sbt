val stageVersion = "0.1"
val junitVersion = "0.11"
val nscalatimeVersion = "2.30.0"
val scalacticVersion = "3.2.10"
val catsVersion = "2.7.0"
val commonIOVersion = "2.8.0"
val scalazVersion = "7.4.0-M10"
val asmVersion = "9.1"
val guavaVersion = "30.1-jre"
val akkaVersion = "2.6.18"
val circeVersion = "0.12.3"
val typesafeConfigVersion = "1.4.1"
val loggerVersion = "1.2.3"

scalaVersion := "3.1.1" //dottyLatestNightlyBuild.get


lazy val root = project
  .in(file("."))
  .settings(
    name := "STAGE",
    version := stageVersion,
  )

resolvers += ("apache_snapshots" at "http://repository.apache.org/snapshots").withAllowInsecureProtocol(true)
resolvers += ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "ch.qos.logback" % "logback-classic" % loggerVersion,
  "org.scala-lang" % "scala-reflect" % "2.13.5",
  "org.scala-lang" % "scala-compiler" % "2.13.5",
  "org.scalactic" %% "scalactic" % scalacticVersion,
  "org.scalatest" %% "scalatest" % scalacticVersion % Test, //).exclude("org.scalactic", "scalactic_2.13"),
  "org.scalatest" %% "scalatest-featurespec" % scalacticVersion % Test, //.exclude("org.scalactic", "scalactic_2.13")
  "com.github.nscala-time" %% "nscala-time" % nscalatimeVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.ow2.asm" % "asm" % asmVersion,
  "org.ow2.asm" % "asm-commons" % asmVersion,
  "org.ow2.asm" % "asm-util" % asmVersion,
  "com.novocode" % "junit-interface" % junitVersion % "test",
  "com.typesafe" % "config" % typesafeConfigVersion,
  "com.google.guava" % "guava" % guavaVersion,
  "commons-io" % "commons-io" % commonIOVersion
)
/*
  .map(_.withDottyCompat(scalaVersion.value))


scalacOptions ++= {
  if (isDotty.value) Seq("-source:3.0-migration") else Nil
}
*/

resolvers += Resolver.url("typesafe", url("https://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
Test / parallelExecution := false
