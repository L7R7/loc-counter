package com.l7r7.loc

import cats.effect.{IO, IOApp, Resource}
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.Pipe
import fs2.io.file.{Files, Path}
import org.http4s.ember.client.EmberClientBuilder

import java.text.SimpleDateFormat
import java.util.Date

object Main extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      config <- Config()
      _ <- IO.println(s"Running with Config: $config")
      tmpDir = if config.keepTmpDir then Resource.eval(Files[IO].createTempDirectory) else Files[IO].tempDirectory
      _ <- (tmpDir, EmberClientBuilder.default[IO].withUserAgent(config.userAgent).build).tupled
        .use((tmpDir, client) =>
          for
            gitlab <- Gitlab(config, tmpDir, client)
            cloc = Cloc(tmpDir)
            persistence <- Persistence(config)
            chart = Chart(config, gitlab)
            _ <- program(config, gitlab, cloc, persistence, chart)
          yield ()
        )
    yield ()

  private def program(config: Config, gitlab: Gitlab, cloc: Cloc, persistence: Persistence, chart: Chart): IO[Unit] = gitlab.getProjects
    .through(cloneAndCountLoc(gitlab, cloc))
    .foldMonoid
    .evalTap(result => IO.println(s"Processing result: ${result.total} LOCs across ${result.languages.size} languages"))
    .through(persistResults(persistence))
    .through(generateChart(config, persistence, chart))
    .compile
    .drain

  private def cloneAndCountLoc(gitlab: Gitlab, cloc: Cloc): Pipe[IO, Project, Result] =
    _.parEvalMapUnordered(25)(project =>
      for
        x <- gitlab.shallowClone(project.http_url_to_repo).timed
        y <- cloc.countLoc(Path(project.path)).timed
        _ <- IO.println(s"${(project.name + ":").padTo(26, ' ')} clone: ${x._1.toSeconds}s,\tcloc: ${y._1.toSeconds}s")
      yield y._2
    )

  private def persistResults(persistence: Persistence): Pipe[IO, Result, Unit] =
    _.evalMap(result =>
      for
        now <- IO.realTimeInstant
        entries = result.languages.toList.map((lang, res) => Entry(lang, Date.from(now), res.total))
        _ <- persistence.persistEntries(entries)
      yield ()
    )

  private def generateChart(config: Config, persistence: Persistence, chart: Chart): Pipe[IO, Unit, Unit] =
    val dateFormat = SimpleDateFormat("yyyy-MM-dd")
    _.flatMap(_ => persistence.getEntriesOrderedByLanguageAndDate)
      .groupAdjacentBy(_.language)
      .map((label, entries) =>
        ChartEntry(
          label = label,
          data = entries.map(e => ChartEntry.DataPoint(dateFormat.format(e.date), e.loc)).toList,
          hidden = !config.defaultLanguages.contains(label)
        )
      )
      .foldMap(List(_))
      .evalMap(entries => chart.renderChart(entries))
