package com.l7r7.loc

import cats.effect.IO
import cats.effect.std.Semaphore
import cats.syntax.all.*
import fs2.io.file.Path
import fs2.io.process.ProcessBuilder
import fs2.text.utf8
import fs2.{Chunk, Stream}
import io.circe.Codec
import org.http4s.*
import org.http4s.Uri.UserInfo
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.Link
import org.typelevel.ci.*

trait Gitlab:
  def shallowClone(url: Uri): IO[Unit]
  def getProjects: Stream[IO, Project]
  def getGroupInformation(groupId: Int): IO[Group]

object Gitlab:
  extension (uri: Uri)
    def setUserInfo(apiToken: String): Uri = uri.copy(authority = uri.authority.map(_.copy(userInfo = Some(UserInfo("token", Some(apiToken))))))

  def apply(config: Config, workingDir: Path, client: Client[IO]): IO[Gitlab] =
    for sem <- Semaphore[IO](2)
    yield new Gitlab:
      override def shallowClone(url: Uri): IO[Unit] =
        sem.permit.use(_ =>
          ProcessBuilder("git", "clone", "--depth", "1", url.setUserInfo(config.apiToken).renderString)
            .withWorkingDirectory(workingDir)
            .spawn[IO]
            .use(process =>
              for
                _ <- process.stdout.compile.drain
                stderr <- process.stderr.through(utf8.decode).compile.string
                exitValue <- process.exitValue
                _ <- IO.raiseError(RuntimeException(s"Git clone failed with exit code $exitValue, error: $stderr")).whenA(exitValue != 0)
              yield ()
            )
        )

      override def getProjects: Stream[IO, Project] =
        val req = Request[IO](
          method = Method.GET,
          uri = config.gitlabApiBaseUrl / "groups" / config.groupId / "projects"
            withQueryParam ("archived", "false")
            withQueryParam ("include_subgroups", "true")
            withQueryParam ("with_shared", "false"),
          headers = Headers(Header.Raw(ci"Private-Token", config.apiToken))
        )
        Stream
          .unfoldChunkLoopEval(req)(req =>
            client
              .run(req)
              .use(response =>
                if response.status == Status.Ok then
                  val nextPage =
                    for
                      link <- response.headers.get[Link]
                      uri <- link.values.find(_.rel.contains("next")).map(_.uri)
                    yield req.withUri(uri)
                  response.as[List[Project]].map(projects => (Chunk.from(projects), nextPage))
                else IO.raiseError(RuntimeException(s"Failed to fetch projects from ${req.uri}: ${response.status}"))
              )
          )
          .prefetch

      def getGroupInformation(groupId: Int): IO[Group] =
        val req = Request[IO](
          method = Method.GET,
          uri = config.gitlabApiBaseUrl / "groups" / groupId,
          headers = Headers(Header.Raw(ci"Private-Token", config.apiToken))
        )
        client.run(req).use { response =>
          if response.status == Status.Ok then response.as[Group]
          else IO.raiseError(RuntimeException(s"Failed to fetch group information from ${req.uri}: ${response.status}"))
        }

case class Project(name: String, path: String, http_url_to_repo: Uri) derives Codec.AsObject

case class Group(id: Int, web_url: Uri, full_name: String) derives Codec.AsObject
