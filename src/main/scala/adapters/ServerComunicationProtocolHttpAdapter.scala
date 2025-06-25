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
import spray.json.RootJsonFormat
import domain.MusicPlayer.MusicPlayerOpsImpl.toSeconds
import domain.MusicPlayer
import scala.util.Using

class ServerComunicationProtocolHttpAdapter(id: String, name: String, clientPort: Int, announcePort: Int, discoveryBroadcastAddress: String)(using ExecutionContext) extends ServerComunicationProtocol:
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

  import spray.json.DefaultJsonProtocol.{*, given}

  case class UpdatePropertyItem(
      propertyId: String,
      value: DomoticASW.ActualTypes
  ) derives Writer

  case class EventItem(
    event: String
  ) derives Writer

  case class MusicPlayerState(
    state: String,
    music: String,
    minutes: String,
    musicProgress: Int
  )

  var prevState: Option[MusicPlayerState] = None

  private def musicProgress(m: Music, t: Milliseconds) =
    (t.toSeconds.toInt / m.duration.toDouble * 100).toInt

  private def musicMinutes(m: Music, t: Milliseconds) =
    t.toSeconds.toMin.asString + "/" + m.duration.toSeconds.toMin.asString

  private def stateFromMusicAndCurrentTime(s: String, m: Music, t: Milliseconds) =
    MusicPlayerState(s.toString(), m.name, musicMinutes(m, t), musicProgress(m, t))

  override def sendEvent(address: ServerAddress, e: Event): Future[Unit] =
    println(write(EventItem(e.toString())))
    quickRequest
      .httpVersion(HttpVersion.HTTP_1_1)
      .post(
        uri"http://${address.host}:${address.port}/api/devices/${this.id}/events"
      )
      .contentType(MediaType.ApplicationJson)
      .body(write(EventItem(e.toString())))
      .send(DefaultFutureBackend())
      .recoverWith(err =>
        Console.err.println(err)
        Future.failed(err)
      )
      .map(_ => ())

  override def updateState(address: ServerAddress, state: MusicState): Future[Unit] =
    val currentState = state match
      case Playing(m, t) => stateFromMusicAndCurrentTime("Playing", m, t)
      case Paused(m, t) => stateFromMusicAndCurrentTime("Paused", m, t)
      case Off(m) => stateFromMusicAndCurrentTime("Off", m, 0.toMs)

    prevState match
      case Some(prev) if prev == currentState => Future(())
      case _ =>
        prevState = Some(currentState)
        val updates = Seq(
          UpdatePropertyItem("state", currentState.state),
          UpdatePropertyItem("musics", currentState.music),
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

  import java.net.{DatagramPacket, DatagramSocket, InetAddress}
  import java.nio.charset.StandardCharsets
  import scala.util.Using

  case class AnnounceMessage(id: String, name: String, port: Int) derives Writer

  override def announce(): Unit =
    Using(DatagramSocket()): socket =>
      socket.setBroadcast(true)
      val data =
        write(AnnounceMessage(id, name, clientPort))
          .getBytes(StandardCharsets.UTF_8)
      val broadcastAddress = InetAddress.getByName(discoveryBroadcastAddress)
      val packet = new DatagramPacket(
        data,
        data.length,
        broadcastAddress,
        announcePort
      )
      socket.send(packet)