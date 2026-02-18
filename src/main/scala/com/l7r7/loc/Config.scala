package com.l7r7.loc

import cats.effect.IO
import cats.effect.std.Env
import cats.syntax.all.*
import fs2.Stream
import fs2.data.csv.lenient.attemptDecodeWithoutHeaders
import fs2.data.text.utf8.byteStreamCharLike
import fs2.io.file.{Files, Path}
import org.http4s.Uri
import org.http4s.headers.`User-Agent`

case class Config(
    apiToken: String,
    gitlabApiBaseUrl: Uri,
    ciProjectUri: Uri,
    userAgent: `User-Agent`,
    groupId: Int,
    databaseName: String,
    defaultLanguages: List[String],
    keepTmpDir: Boolean,
    markers: List[Marker]
):
  override def toString: String =
    s"Config(apiToken=****, gitlabApiBaseUrl=$gitlabApiBaseUrl, userAgent=$userAgent, groupId=$groupId, databaseName=$databaseName, defaultLanguages=${defaultLanguages.mkString(", ")}, keepTmpDir=$keepTmpDir, markers=$markers)"

object Config:
  extension (envVariable: String)
    def lookupFromEnv(): IO[String] = parseFromEnv(Some(_))
    def parseFromEnv[A](f: String => Option[A], fallback: Option[String] = None): IO[A] =
      for
        maybeEnv <- Env[IO].get(envVariable)
        s <- IO.fromOption(maybeEnv.orElse(fallback))(RuntimeException(s"Environment variable $envVariable not set"))
        res <- IO.fromOption(f(s))(RuntimeException(s"$envVariable can't be parsed"))
      yield res

  def apply(): IO[Config] =
    for
      apiToken <- "GITLAB_API_TOKEN".lookupFromEnv()
      gitlabApiBaseUrl <- "CI_API_V4_URL".parseFromEnv(Uri.fromString(_).toOption)
      ciProjectUri <- "CI_PROJECT_URL".parseFromEnv(Uri.fromString(_).toOption)
      userAgent <- "USER_AGENT".parseFromEnv(s => `User-Agent`.parse(10)(s).toOption, Some("loc-counter"))
      groupId <- "GITLAB_GROUP_ID".parseFromEnv(_.toIntOption)
      databaseName <- "DATABASE_NAME".lookupFromEnv()
      defaultLanguages <- "DEFAULT_LANGUAGES".parseFromEnv(s => Some(s.split(",").toList), Some(""))
      keepTmpDir <- "KEEP_TMP_DIR".parseFromEnv(_.toBooleanOption, Some("false"))
      markers <- parseMarkers
    yield Config(apiToken, gitlabApiBaseUrl, ciProjectUri, userAgent, groupId, databaseName, defaultLanguages, keepTmpDir, markers)

  def parseMarkers: IO[List[Marker]] =
    val markersPath = Path("markers.csv")
    for
      fileExists <- Files[IO].exists(markersPath)
      input =
        if fileExists then Files[IO].readAll(markersPath)
        else Stream.exec(IO.println("markers.csv not found, proceeding with an empty list of markers"))
      parsed <- input.through(attemptDecodeWithoutHeaders[Marker](separator = ';')).compile.toList
      res <- parsed.sequence match
        case Left(err)      => IO.raiseError(RuntimeException("Failed to parse markers.csv", err))
        case Right(markers) => IO.pure(markers)
    yield res
