package adapters

import scala.concurrent.Future

import org.apache.pekko
import pekko.actor.typed.ActorSystem
import pekko.http.scaladsl.Http
import pekko.http.scaladsl.Http.ServerBinding
import pekko.http.scaladsl.model._
import pekko.http.scaladsl.server.Directives._
import pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.{*, given}
import spray.json.DefaultJsonProtocol.{*, given}
import spray.json.*
import java.util.UUID
import domoticasw.DomoticASW.*
import domain.MusicPlayerAgent
import domain.Action
import Action.*
import domain.MusicPlayerState.*
import domain.Event
import utils.OneOf.More
import org.apache.pekko.stream.scaladsl.Sink
import ports.ServerComunicationProtocol.ServerAddress
import domain.MusicPlayer.MusicPlayerOpsImpl.toMin
import domain.MusicPlayer.MusicPlayerOpsImpl.toSeconds

object DomoticASWDeviceHttpInterface:
  import Marshalling.given
  case class BadRequest(message: String)
  case class NotFound(message: String)
  case class ExecuteActionBody(input: Option[String])
  case class RegisterBody(serverPort: Int)

  def badActionIdMessage(action: String) =
    s"Action \"$action\" not found, known actions are [\"changeMusic\", \"setMusicProgress\", \"play\", \"pause\", \"stop\"]"

  def apply(host: String, port: Int, musicPlayerAgent: MusicPlayerAgent)(
    using a: ActorSystem[Any]
  ): Future[ServerBinding] =
    Http()
      .newServerAt(host, port)
      .connectionSource()
      .to {
        Sink foreach: conn =>
          val clientAddress = conn.remoteAddress
          conn.handleWithAsyncHandler:
            concat(
              (path("execute" / Segment) & entity(as[ExecuteActionBody])):
                (segment, body) =>
                  val action: Either[BadRequest, Action] = segment match
                    case "play" => Right(Play)
                    case "pause" => Right(Pause)
                    case "stop" => Right(Stop)
                    case "change-music" =>
                      body.input match
                        case Some(value) if musicPlayerAgent.musicPlayer.musics.find(_.name == value).isDefined =>
                          Right(ChangeMusic(musicPlayerAgent.musicPlayer.musics.find(_.name == value).get))
                        case _ => Left(BadRequest("The chosen music does not exists"))
                    case "set-music-progress" =>
                      body.input match
                        case Some(value) if value.toIntOption.isDefined => 
                          value.toInt match
                            case x if x > 100 || x < 0 => Left(BadRequest("The music progress must be between 0 and 100"))
                            case _ => Right(ChangeTime(value.toInt))
                        case _ => Left(BadRequest("The new music progress should exists if trying to change the old one"))
                  
                  post:
                    action match
                      case Left(err) => 
                        complete(StatusCodes.BadRequest, err)
                      case Right(value) =>
                        musicPlayerAgent.enqueAction(value)
                        complete(StatusCodes.OK)
              ,
              (path("register") & entity(as[RegisterBody]) & post): body =>
                musicPlayerAgent.registerToServer(ServerAddress(clientAddress.getHostName(), body.serverPort))
                complete(StatusCodes.OK, musicPlayerRegistration(musicPlayerAgent))
              ,
              path("check-status"):
                complete(StatusCodes.OK)
            )
      }
    .run()

  def musicPlayerRegistration(a: MusicPlayerAgent) = DeviceRegistration(
    a.musicPlayer.id,
    a.musicPlayer.name,
    Seq(
      DeviceProperty.WithTypeConstraint(
        "state",
        "Music state",
        a.musicPlayer.initialState match
          case More(s, tail) => s match
            case Off(_) => "Off"
            case Playing(_, _) => "Playing"
            case Paused(_, _) => "Paused"
        ,
        TypeConstraints.Enum(Set("Playing", "Paused", "Off"))
      ),
      DeviceProperty.WithSetter(
        "musics",
        "Musics",
        a.musicPlayer.initialState match
          case More(s, tail) => s match
            case Off(m) => m.name
            case Playing(m, _) => m.name
            case Paused(m, _) => m.name
        ,
        "change-music"
      ),
      DeviceProperty.WithTypeConstraint(
        "minutes",
        "Minutes",
        a.musicPlayer.initialState match
          case More(s, tail) => s match
            case Playing(m, t) => t.toSeconds.toMin.asString + "/" + m.duration.toSeconds.toMin.asString
            case Paused(m, t) => t.toSeconds.toMin.asString + "/" + m.duration.toSeconds.toMin.asString
            case Off(m) => "00:00/" + m.duration.toSeconds.toMin.asString
        ,
        TypeConstraints.None(Type.String)
      ),
      DeviceProperty.WithSetter(
        "music-progress",
        "Music progress",
        a.musicPlayer.initialState match
          case More(s, tail) => s match
            case Playing(m, t) => (t.toSeconds.toInt / m.duration.toDouble * 100).toInt
            case Paused(m, t) => (t.toSeconds.toInt / m.duration.toDouble * 100).toInt
            case _ => 0
        ,
        "set-music-progress"
      )
    ),
    Seq(
      DeviceAction(
        "change-music",
        "Change music",
        Some("Change the music to play"),
        TypeConstraints.Enum(a.musicPlayer.musics.map(_.name))
      ),
      DeviceAction(
        "set-music-progress",
        "Set music progress",
        None,
        TypeConstraints.IntRange(0, 100)
      ),
      DeviceAction(
        "play",
        "Play",
        None,
        TypeConstraints.None(Type.Void)
      ),
      DeviceAction(
        "pause",
        "Pause",
        None,
        TypeConstraints.None(Type.Void)
      ),
      DeviceAction(
        "stop",
        "Stop",
        None,
        TypeConstraints.None(Type.Void)
      )
    ),
    Event.values.toIndexedSeq.map(_.toString())
  )

  object Marshalling:
    import DomoticASWDeviceHttpInterface.*
    given RootJsonFormat[BadRequest] = jsonFormat1(BadRequest.apply)
    given RootJsonFormat[NotFound] = jsonFormat1(NotFound.apply)
    given RootJsonFormat[ExecuteActionBody] = jsonFormat1(ExecuteActionBody.apply)
    given RootJsonFormat[RegisterBody] = jsonFormat1(RegisterBody.apply)

    given RootJsonFormat[Color] = jsonFormat3(Color.apply)
    given RootJsonFormat[Type] = new RootJsonFormat {
      def read(json: JsValue): Type =
        val typeOpt = json match
          case JsString(s) => Type.fromString(s)
          case _           => None
        typeOpt.getOrElse:
          val values = Type.values.mkString(", ")
          deserializationError(s"Available values for type are: ${values}")

      def write(obj: Type): JsValue = JsString(obj.toString())
    }

    given RootJsonFormat[ActualTypes] = new RootJsonFormat {
      def read(json: JsValue): ActualTypes = json match
        case JsObject(f) =>
          val colorOpt = for
            r <- f.get("r")
            r <-
              if r.isInstanceOf[JsNumber] then Some(r.asInstanceOf[JsNumber])
              else scala.None
            g <- f.get("g")
            g <-
              if g.isInstanceOf[JsNumber] then Some(g.asInstanceOf[JsNumber])
              else scala.None
            b <- f.get("b")
            b <-
              if b.isInstanceOf[JsNumber] then Some(b.asInstanceOf[JsNumber])
              else scala.None
          yield (Color(r.value.toInt, g.value.toInt, b.value.toInt))
          colorOpt.getOrElse(deserializationError("Expected Color"))
        case JsString(value)                  => value
        case JsNumber(value) if value.isWhole => value.toInt
        case JsNumber(value)                  => value.toDouble
        case JsNull                           => ()
        case JsTrue                           => true
        case JsFalse                          => false
        case _ =>
          deserializationError(
            "Expected one of [\"Color\",\"String\",\"Int\",\"Double\",\"Null\",\"Boolean\",]"
          )

      def write(obj: ActualTypes): JsValue = obj match
        case obj: Unit    => JsNull
        case obj: Color   => summon[JsonWriter[Color]].write(obj)
        case obj: Boolean => summon[JsonWriter[Boolean]].write(obj)
        case obj: Double  => summon[JsonWriter[Double]].write(obj)
        case obj: Int     => summon[JsonWriter[Int]].write(obj)
        case obj: String  => summon[JsonWriter[String]].write(obj)
    }

    import TypeConstraints.*
    given RootJsonFormat[Enum] = jsonFormat1(Enum.apply)
    given RootJsonFormat[IntRange] = jsonFormat2(IntRange.apply)
    given RootJsonFormat[DoubleRange] = jsonFormat2(DoubleRange.apply)
    given RootJsonFormat[None] = jsonFormat1(None.apply)
    given RootJsonFormat[TypeConstraints] = new RootJsonFormat {
      def read(json: JsValue): TypeConstraints =
        json.asJsObject().fields.get("constraint") match
          case scala.None => deserializationError("expected field \"constraint\"")
          case Some(value) =>
            val fields = JsObject(json.asJsObject().fields - "constraint")
            value match
              case JsString("None") => summon[JsonReader[None]].read(fields)
              case JsString("Enum") => summon[JsonReader[Enum]].read(fields)
              case JsString("IntRange") =>
                summon[JsonReader[IntRange]].read(fields)
              case JsString("DoubleRange") =>
                summon[JsonReader[DoubleRange]].read(fields)
              case JsString(_) =>
                deserializationError(
                  "One of [\"None\", \"Enum\", \"IntRange\", \"Double\"] expected for field \"constraint\""
                )
              case _ =>
                deserializationError("String expected for field \"constraint\"")

      def write(obj: TypeConstraints): JsValue =
        obj match
          case Enum(values) =>
            JsObject(
              Map(
                ("constraint" -> JsString("Enum")),
                ("values" -> JsArray(values.map(JsString(_)).toSeq*))
              )
            )
          case IntRange(min, max) =>
            JsObject(
              Map(
                ("constraint" -> JsString("IntRange")),
                ("min" -> JsNumber(min)),
                ("max" -> JsNumber(max))
              )
            )
          case DoubleRange(min, max) =>
            JsObject(
              Map(
                ("constraint" -> JsString("DoubleRange")),
                ("min" -> JsNumber(min)),
                ("max" -> JsNumber(max))
              )
            )
          case None(t) =>
            JsObject(
              Map(
                ("constraint" -> JsString("None")),
                ("type" -> JsString(t.toString()))
              )
            )

    }

    import DeviceProperty.*
    given RootJsonFormat[WithSetter] = jsonFormat4(WithSetter.apply)
    given RootJsonFormat[WithTypeConstraint] =
      jsonFormat4(WithTypeConstraint.apply)
    given RootJsonFormat[DeviceProperty] = new RootJsonFormat {
      def read(json: JsValue): DeviceProperty =
        val fields = json.asJsObject.fields
        fields.contains("setterActionId") match
          case true => summon[JsonReader[WithSetter]].read(json)
          case false if fields.contains("typeConstraints") =>
            summon[JsonReader[WithTypeConstraint]].read(json)
          case false =>
            deserializationError(
              "Expected object containing one of these fields: [\"setterActionId\", \"typeConstraints\"]"
            )

      def write(obj: DeviceProperty): JsValue = obj match
        case obj: WithSetter =>
          summon[JsonFormat[WithSetter]].write(obj)
        case obj: WithTypeConstraint =>
          summon[JsonFormat[WithTypeConstraint]].write(obj)
    }

    given RootJsonFormat[DeviceAction] = jsonFormat4(DeviceAction.apply)

    given RootJsonFormat[DeviceRegistration] =
      jsonFormat5(DeviceRegistration.apply)