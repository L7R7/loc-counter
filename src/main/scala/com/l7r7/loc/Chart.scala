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
import io.circe.{Codec, Decoder, Encoder, KeyEncoder}

import java.io.StringWriter
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, ZoneId}
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
            sortedEntries = entries.sortBy(_.label)
            markersMap = config.markers.map(m => m.value -> m).toMap
            model = Map(
              "chartData" -> sortedEntries.asJson,
              "markers" -> markersMap.asJson,
              "generatedAt" -> now.asHumanReadbleString,
              "groupFullName" -> group.full_name,
              "groupUrl" -> group.web_url,
              "ciProjectUri" -> config.ciProjectUri
            )
            res <- IO.blocking {
              val pebbleEngine = ScalaPebbleEngine()
              val template = pebbleEngine.loadTemplate("chart.peb")
              val writer = StringWriter()
              template.evaluate(writer, model.asJava)
              writer.toString
            }
          yield res
        )
        .through(utf8.encode)
        .through(Files[IO].writeAll(Path("chart.html")))
        .compile
        .drain

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

    extension (dateString: DateString) def asHumanReadableString: String = dateString.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))

  extension (instant: Instant)
    def asHumanReadbleString: String = instant.atZone(ZoneId.of("Europe/Berlin")).format(DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss"))

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
