package com.l7r7.loc

import cats.effect.IO
import cats.syntax.all.*
import com.l7r7.loc.Marker.*
import com.sfxcode.templating.pebble.ScalaPebbleEngine
import fs2.Stream
import fs2.data.csv.generic.semiauto.deriveRowDecoder
import fs2.data.csv.{CellDecoder, DecoderError, RowDecoder}
import fs2.io.file.{Files, Path}
import fs2.text.utf8
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, Json, KeyEncoder}

import java.io.StringWriter
import java.text.NumberFormat
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, ZoneId}
import java.util.Locale
import scala.jdk.CollectionConverters.*

trait Chart:
  def renderChart(entries: List[ChartEntry]): IO[Unit]

object Chart:
  def apply(config: Config, gitlab: Gitlab): Chart =
    (entries: List[ChartEntry]) =>
      Stream
        .eval(
          for
            now <- IO.realTimeInstant
            group <- gitlab.getGroupInformation(config.groupId)
            model = buildModel(entries, group.full_name, group.web_url.renderString, config.ciProjectUri.renderString, config.markers, now)
            res <- IO.blocking {
              val pebbleEngine = ScalaPebbleEngine()
              val template = pebbleEngine.loadTemplate("chart.peb")
              val writer = StringWriter()
              template.evaluate(writer, model)
              writer.toString
            }
          yield res
        )
        .through(utf8.encode)
        .through(Files[IO].writeAll(Path("chart.html")))
        .compile
        .drain

  private def buildModel(
      entries: List[ChartEntry],
      groupFullName: String,
      groupUrl: String,
      ciProjectUri: String,
      markers: List[Marker],
      now: Instant
  ): java.util.Map[String, Any] =
    val sortedEntries = entries.sortBy(_.label)
    val colors = palette(sortedEntries.size)

    val chartData = sortedEntries
      .zip(colors)
      .map { (entry, color) =>
        entry.asJson.deepMerge(
          Json.obj(
            "borderColor" -> color.asJson,
            "backgroundColor" -> color.asJson,
            "pointBackgroundColor" -> color.asJson
          )
        )
      }
      .asJson

    val markersMap = markers.map(m => m.value -> m).toMap

    val langInfos = sortedEntries.zip(colors).zipWithIndex.map { case ((entry, color), index) =>
      (index, entry.label, color, entry.data.lastOption.map(_.y).getOrElse(0L), entry.hidden)
    }

    val totalLatest = langInfos.map(_._4).sum

    val allDates = sortedEntries.flatMap(_.data.map(dp => LocalDate.parse(dp.x)))
    val earliest = allDates.minByOption(_.toEpochDay)
    val latest = allDates.maxByOption(_.toEpochDay)

    val total30Ago = latest.fold(0L) { last =>
      val target = last.minusDays(30)
      sortedEntries.flatMap(_.data.filter(dp => !LocalDate.parse(dp.x).isAfter(target)).lastOption.map(_.y)).sum
    }
    val delta = totalLatest - total30Ago

    val monthFmt = DateTimeFormatter.ofPattern("MMM yyyy", Locale.ENGLISH)
    val dayFmt = DateTimeFormatter.ofPattern("MMM d, yyyy", Locale.ENGLISH)

    val stats = Map[String, Any](
      "total" -> fmtCompact(totalLatest),
      "langs" -> sortedEntries.size.toString,
      "since" -> earliest.map(_.format(monthFmt)).getOrElse("—"),
      "latest" -> latest.map(_.format(dayFmt)).getOrElse("—"),
      "trend" -> s"${if delta >= 0 then "+" else "−"}${fmtCompact(math.abs(delta))} · 30d"
    ).asJava

    val languages = langInfos
      .sortBy(-_._4)
      .map { (index, label, color, latestVal, hidden) =>
        val share = if totalLatest > 0 then latestVal.toDouble / totalLatest * 100 else 0.0
        Map[String, Any](
          "index" -> index.toString,
          "name" -> label,
          "color" -> color,
          "count" -> fmtInt(latestVal),
          "share" -> String.format(Locale.US, if share >= 10 then "%.0f" else "%.1f", share),
          "barWidth" -> String.format(Locale.US, "%.2f", math.max(share, 0.6)),
          "hidden" -> Boolean.box(hidden)
        ).asJava
      }
      .asJava

    val tagRegex = "^([A-Z]+-\\d+):\\s*(.*)$".r
    val milestones = markers
      .sortBy(_.value.asKey)(using Ordering[String].reverse)
      .map { m =>
        val (tag, text) = m.label match
          case tagRegex(t, rest) => (t, rest)
          case other             => ("", other)
        Map[String, Any](
          "key" -> m.value.asKey,
          "date" -> m.value.asMilestoneDate,
          "tag" -> tag,
          "text" -> text
        ).asJava
      }
      .asJava

    Map[String, Any](
      "chartData" -> chartData,
      "markers" -> markersMap.asJson,
      "stats" -> stats,
      "languages" -> languages,
      "milestones" -> milestones,
      "generatedAt" -> now.asHumanReadbleString,
      "groupFullName" -> groupFullName,
      "groupUrl" -> groupUrl,
      "ciProjectUri" -> ciProjectUri
    ).asJava

  private def palette(n: Int): List[String] =
    var hue = 210.0
    (0 until n).map { i =>
      val sat = 62 + (i % 3) * 6
      val light = 52 + (i % 2) * 6
      val color = s"hsl(${math.round(hue) % 360} $sat% $light%)"
      hue += 137.508
      color
    }.toList

  private def fmtInt(n: Long): String = NumberFormat.getIntegerInstance(Locale.US).format(n)

  private def fmtCompact(n: Long): String =
    val a = math.abs(n)
    if a >= 1000000 then String.format(Locale.US, if a >= 10000000 then "%.0f" else "%.2f", n.toDouble / 1000000) + "M"
    else if a >= 1000 then String.format(Locale.US, if a >= 10000 then "%.0f" else "%.1f", n.toDouble / 1000) + "k"
    else n.toString

case class ChartEntry(label: String, data: List[ChartEntry.DataPoint], hidden: Boolean = false) derives Codec.AsObject

object ChartEntry:
  case class DataPoint(x: String, y: Long) derives Codec.AsObject

case class Marker(value: DateString, label: String)

object Marker:
  given RowDecoder[Marker] = deriveRowDecoder

  opaque type DateString = LocalDate
  object DateString:
    private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    given Encoder[DateString] = Encoder.encodeString.contramap(_.format(formatter))
    given KeyEncoder[DateString] = KeyEncoder.encodeKeyString.contramap(_.format(formatter))
    given CellDecoder[DateString] = CellDecoder.stringDecoder.emap(s =>
      Either
        .catchNonFatal(LocalDate.parse(s, formatter))
        .leftMap(err => DecoderError(s"failed to parse marker value '$s' as yyyy-MM-dd date", inner = err))
    )

    extension (dateString: DateString)
      def asHumanReadableString: String = dateString.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))
      def asKey: String = dateString.format(formatter)
      def asMilestoneDate: String = dateString.format(DateTimeFormatter.ofPattern("dd MMM yy", Locale.ENGLISH))

  extension (instant: Instant)
    def asHumanReadbleString: String = instant.atZone(ZoneId.of("Europe/Berlin")).format(DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm"))

  given Encoder[Marker] = Encoder.instance(marker =>
    Map(
      "type" -> "line".asJson,
      "mode" -> "vertical".asJson,
      "scaleID" -> "x".asJson,
      "value" -> marker.value.asJson,
      "borderWidth" -> 3.asJson,
      "label" -> Map(
        "content" -> s"${marker.value.asHumanReadableString}: ${marker.label}".asJson,
        "display" -> false.asJson,
        "position" -> "start".asJson
      ).asJson
    ).asJson
  )
