package com.l7r7.loc

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import fs2.Stream

import java.text.SimpleDateFormat
import java.util.Date

trait Persistence:
  def persistEntries(entries: List[Entry]): IO[Unit]
  def getEntriesOrderedByLanguageAndDate: Stream[IO, Entry]

object Persistence:
  private val dateFormat = SimpleDateFormat("yyyy-MM-dd")
  given Read[Entry] = Read[(String, String, Long)].map((language, dateStr, loc) => Entry(language, dateFormat.parse(dateStr), loc))
  given Write[Entry] = Write[(String, String, Long)].contramap(entry => (entry.language, dateFormat.format(entry.date), entry.loc))

  def apply(config: Config): IO[Persistence] =
    val xa = Transactor.fromDriverManager[IO]("org.sqlite.JDBC", s"jdbc:sqlite:${config.databaseName}", logHandler = None)
    for _ <-
        sql"CREATE TABLE IF NOT EXISTS loc_entries (language TEXT NOT NULL, date TEXT NOT NULL, loc INTEGER NOT NULL, PRIMARY KEY (language, date))".update.run
          .transact(xa)
    yield new Persistence:
      override def persistEntries(entries: List[Entry]): IO[Unit] =
        val sql = "INSERT INTO loc_entries (language, date, loc) VALUES (?, ?, ?) ON CONFLICT (language, date) DO UPDATE SET loc=excluded.loc"
        Update[Entry](sql).updateMany(entries).transact(xa).void

      override def getEntriesOrderedByLanguageAndDate: Stream[IO, Entry] =
        sql"SELECT language, date, loc FROM loc_entries ORDER BY language, date".query[Entry].stream.transact(xa)

case class Entry(language: String, date: Date, loc: Long)
