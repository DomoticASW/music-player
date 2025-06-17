package adapters

import ports.ServerComunicationProtocol.ServerComunicationProtocol
import domain.Event
import domain.MusicPlayer.MusicPlayerOpsImpl.MusicState
import ports.ServerComunicationProtocol.ServerAddress
import scala.concurrent.*
import sttp.model.*
import sttp.client4.quick.*
import sttp.client4.DefaultFutureBackend
import upickle.default.*
import domoticasw.DomoticASW
import domoticasw.DomoticASW.ActualTypes
import upickle.core.Visitor
import domoticasw.DomoticASW.Color
import domain.Music
import domain.MusicPlayerState.Playing
import domain.MusicPlayerState.Paused
import domain.MusicPlayerState.Off
import domain.MusicPlayer.MusicPlayerOpsImpl.Milliseconds
import domain.MusicPlayer.MusicPlayerOpsImpl.toMs

class ServerComunicationProtocolHttpAdapter(id: String)(using ExecutionContext) extends ServerComunicationProtocol:
  given Writer[Color] = Writer.derived
  given Writer[DomoticASW.ActualTypes] with
    def write0[V](out: Visitor[?, V], v: ActualTypes): V =
      v match
        case obj: Unit   => out.visitNull(-1)
        case obj: Color  => summon[Writer[Color]].write0(out, obj)
        case true        => out.visitTrue(-1)
        case false       => out.visitFalse(-1)
        case obj: Double => out.visitFloat64(obj, -1)
        case obj: Int    => out.visitInt32(obj, -1)
        case obj: String => out.visitString(obj, -1)

  case class UpdatePropertyItem(
      propertyId: String,
      value: DomoticASW.ActualTypes
  ) derives Writer

  case class MusicPlayerState(
    state: domain.MusicPlayerState,
    music: Music,
    minutes: String,
    musicProgress: Int
  )

  var prevState: Option[MusicPlayerState] = None

  private def musicProgress(m: Music, t: Milliseconds) =
    (t.toSeconds.toInt / m.duration.toDouble * 100).toInt

  private def musicMinutes(m: Music, t: Milliseconds) =
    t.toSeconds.toString() + "/" + m.duration

  private def stateFromMusicAndCurrentTime(s: domain.MusicPlayerState, m: Music, t: Milliseconds) =
    MusicPlayerState(s, m, musicMinutes(m, t), musicProgress(m, t))

  override def sendEvent(address: ServerAddress, e: Event): Future[Unit] = 
    quickRequest
      .httpVersion(HttpVersion.HTTP_1_1)
      .patch(
        uri"http://${address.host}:${address.port}/api/devices/${this.id}/events"
      )
      .contentType(MediaType.ApplicationJson)
      .body(write("{ \"event\": " + "\""+ e.toString() + "\""))
      .send(DefaultFutureBackend())
      .recoverWith(err =>
        Console.err.println(err)
        Future.failed(err)
      )
      .map(_ => ())

  override def updateState(address: ServerAddress, state: MusicState): Future[Unit] =
    val currentState = state match
      case s @ Playing(m, t) => stateFromMusicAndCurrentTime(s, m, t)
      case s @ Paused(m, t) => stateFromMusicAndCurrentTime(s, m, t)
      case s @ Off(m) => stateFromMusicAndCurrentTime(s, m, 0.toMs)

    prevState match
      case Some(prev) if prev == currentState => Future(())
      case _ =>
        prevState = Some(currentState)
        val updates = Seq(
          UpdatePropertyItem("state", currentState.state.toString()),
          UpdatePropertyItem("musics", currentState.music.toString()),
          UpdatePropertyItem("minutes", currentState.minutes),
          UpdatePropertyItem("music-progress", currentState.musicProgress),
        )

        quickRequest
          .httpVersion(HttpVersion.HTTP_1_1)
          .patch(
            uri"http://${address.host}:${address.port}/api/devices/${this.id}/properties"
          )
          .contentType(MediaType.ApplicationJson)
          .body(write(updates))
          .send(DefaultFutureBackend())
          .recoverWith(err =>
            Console.err.println(err)
            Future.failed(err)
          )
          .map(_ => ())
