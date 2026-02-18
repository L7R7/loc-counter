package com.l7r7.loc

import cats.Eq
import cats.effect.IO
import cats.kernel.Monoid
import cats.syntax.all.*
import fs2.data.json.*
import fs2.data.json.circe.*
import fs2.data.text.utf8.*
import fs2.io.file.Path
import fs2.io.process.ProcessBuilder
import fs2.text.utf8
import io.circe.{Codec, Decoder, Json}

trait Cloc:
  def countLoc(path: Path): IO[Result]

object Cloc:
  def apply(workingDir: Path): Cloc = (path: Path) =>
    ProcessBuilder("cloc", "--json", "--force-lang=HTML,peb", ".")
      .withWorkingDirectory(workingDir / path)
      .spawn[IO]
      .use(process =>
        for
          maybeResult <- process.stdout
            .through(ast.parse)
            .map(json => if json == Json.obj() then Right(Monoid[Result].empty) else json.as[Result])
            .compile
            .last
          stderr <- process.stderr.through(utf8.decode).compile.string
          exitValue <- process.exitValue
          res <-
            if exitValue == 0
            then
              maybeResult match
                case None => IO.raiseError(RuntimeException(s"Cloc exited successfully, but produced no output, path: $path, error: $stderr"))
                case Some(Left(parseError)) =>
                  IO.raiseError(
                    RuntimeException(s"Cloc exited successfully, but its output can't be parsed: $parseError, path: $path, error: $stderr")
                  )
                case Some(Right(result)) => IO.pure(result)
            else IO.raiseError(RuntimeException(s"Cloc failed with exit code $exitValue, error: $stderr"))
        yield res
      )

case class Result(total: Long, languages: Map[String, Result.SingleResult])

object Result:
  given Eq[Result] = Eq.fromUniversalEquals

  given Monoid[Result] = new Monoid[Result]:
    def empty: Result = Result(total = 0, languages = Map.empty)
    def combine(x: Result, y: Result): Result = Result(
      total = x.total + y.total,
      languages = x.languages.foldLeft(y.languages)((acc, kv) =>
        acc.updatedWith(kv._1) {
          case Some(existing) => Some(existing.combine(kv._2))
          case None           => Some(kv._2)
        }
      )
    )

  given Decoder[Result] = Decoder.instance(cursor =>
    for
      total <- cursor.downField("header").downField("n_lines").as[Long]
      keyValues <- cursor.as[Map[String, Json]]
      languages <- keyValues
        .removedAll(List("header", "SUM"))
        .toList
        .traverse((key, json) => json.as[SingleResult].map(result => key -> result))
        .map(_.toMap)
    yield Result(total, languages)
  )

  case class SingleResult(nFiles: Long, blank: Long, comment: Long, code: Long) derives Codec.AsObject:
    def total: Long = blank + comment + code

  given Monoid[SingleResult] = new Monoid[SingleResult]:
    def empty: SingleResult = SingleResult(nFiles = 0, blank = 0, comment = 0, code = 0)
    def combine(x: SingleResult, y: SingleResult): SingleResult =
      SingleResult(nFiles = x.nFiles + y.nFiles, blank = x.blank + y.blank, comment = x.comment + y.comment, code = x.code + y.code)
