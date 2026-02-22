val circeVersion = "0.14.15"
val doobieVersion = "1.0.0-RC12"
val fs2DataVersion = "1.12.0"
val http4sOtel4sMiddlewareVersion = "0.16.0"
val http4sVersion = "0.23.33"
val log4catsVersion = "2.7.1"
val logbackVersion = "1.5.32"
val openTelemetryLogbackAppenderVersion = "2.19.0-alpha"
val openTelemetryVersion = "1.59.0"
val otel4sExperimentalVersion = "0.9.0"
val otel4sVersion = "0.15.1"
val pebbleScalaVersion = "1.1.6"
val sqliteJdbcVersion = "3.51.2.0"
val typelevelScalafixVersion = "0.5.0"
val weaverVersion = "0.11.3"

lazy val root = (project in file("."))
  .settings(
    organization := "com.l7r7",
    name := "loc-counter",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.8.1",
    Compile / run / fork := true,
    crossTarget := target.value / "scala",
    scalafixOnCompile := true,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    ThisBuild / scalafixDependencies += "org.typelevel" %% "typelevel-scalafix" % typelevelScalafixVersion,
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "com.sfxcode.templating" %% "pebble-scala" % pebbleScalaVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.opentelemetry" % "opentelemetry-exporter-otlp" % openTelemetryVersion % Runtime,
      "io.opentelemetry" % "opentelemetry-sdk-extension-autoconfigure" % openTelemetryVersion % Runtime,
      "io.opentelemetry.instrumentation" % "opentelemetry-logback-appender-1.0" % openTelemetryLogbackAppenderVersion % Runtime,
      "org.gnieh" %% "fs2-data-csv" % fs2DataVersion,
      "org.gnieh" %% "fs2-data-csv-generic" % fs2DataVersion,
      "org.gnieh" %% "fs2-data-json-circe" % fs2DataVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-otel4s-middleware-trace-client" % http4sOtel4sMiddlewareVersion,
      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.typelevel" %% "log4cats-slf4j" % log4catsVersion,
      "org.typelevel" %% "otel4s-experimental-metrics" % otel4sExperimentalVersion,
      "org.typelevel" %% "otel4s-instrumentation-metrics" % otel4sVersion,
      "org.typelevel" %% "otel4s-oteljava" % otel4sVersion,
      "org.typelevel" %% "weaver-cats" % weaverVersion % Test,
      "org.typelevel" %% "weaver-scalacheck" % weaverVersion % Test,
      "org.xerial" % "sqlite-jdbc" % sqliteJdbcVersion
    ),
    assembly / assemblyJarName := "app.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "okio.kotlin_module")   => MergeStrategy.last
      case path if path.matches(".*module-info.class$") => MergeStrategy.last
      case "module-info.class"                          => MergeStrategy.discard
      case x                                            => (assembly / assemblyMergeStrategy).value.apply(x)
    }
  )
