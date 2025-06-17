import domain.Music
import domain.MusicPlayer
import logger.* 
import sleeper.*
import domain.*
import utils.OneOf.*
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import adapters.DomoticASWDeviceHttpInterface
import scala.concurrent.ExecutionContext
import adapters.ServerComunicationProtocolHttpAdapter
import ports.ServerComunicationProtocol.ServerAddress

object Main extends App:
  def musics: Either[String, Set[Music]] =
    for
      musicsStr <- Right(sys.env.get("MUSICS").map(_.split(",").map(_.trim()).toSet))
      musics <- musicsStr match
        case None => Right(Set(
            Music("Back In Black", 10),
            Music("Don't Stop Believin", 5),
            Music("Poker's Face", 7)
          ))
        case Some(value) if value.size > 1 =>
          val pairs = value.map(_.split("-"))
          if pairs.find(_.length != 2).isDefined || pairs.find(!_.apply(1).toIntOption.isDefined).isDefined
          then Left("The Musics are defined by the name and the duration (integer) separeted by a -")
          else Right(pairs.map(p => Music(p(0), p(1).toInt)).toSet)
        case _ => Left("At least one music should be given")
    yield musics

  def steps: Either[String, Long] =
    for
      stepsStr <- Right(sys.env.get("STEPS"))
      steps <- stepsStr match
        case None => Right(1000l)
        case Some(value) =>
          value.toLongOption.toRight("Steps should be an integer")
    yield steps

  object isInt:
    def unapply(s: String): Option[Int] = s.toIntOption

  def serverAddress(
      default: Option[ServerAddress]
  ): Either[String, Option[ServerAddress]] =
    def stringToServerAddress(s: String): Either[String, ServerAddress] =
      s.split(":").toList match
        case host :: (isInt(port) :: next) => Right(ServerAddress(host, port))
        case _ => Left(s"Invalid server address \"$s\"")

    for
      serverAddressStr <- Right(sys.env.get("SERVER_ADDRESS"))
      serverAddress <- serverAddressStr match
        case Some(value) => stringToServerAddress(value).map(Some(_))
        case None        => Right(default)
    yield (serverAddress)

  def port(default: Int): Either[String, Int] =
    sys.env.get("PORT") match
      case None                                  => Right(default)
      case Some(isInt(p)) if p >= 0 & p <= 65335 => Right(p)
      case Some(isInt(p)) => Left(s"Invalid port $p is out of valid port range")
      case Some(nonInt)   => Left(s"Invalid port $nonInt is not an integer")

  def probabilityToPause = 0.2

  def id: Either[String, String] = Right(sys.env.get("ID").getOrElse("Music-player"))
  def musicPlayerName: Either[String, String] = Right(sys.env.get("NAME").getOrElse("Music player"))

  val config = for
    id <- id
    name <- musicPlayerName
    m <- musics
    s <- steps
    port <- port(default = 8080)
    serverAddress <- serverAddress(default = None)
    config <- ConfigChecker(id, name, m, s).left.map(_.message)
  yield (config, port, serverAddress)

  config match
    case Left(err: String) =>
      Console.err.println(err)
      sys.exit(1)
    case Right((config, port, serverAddress)) =>
      val id = config.id
      val name = config.name
      val m = config.musics
      val s = config.steps

      val ec = ExecutionContext.global
      val player = MusicPlayer(id, name, m, s)
      val playerAgent = MusicPlayerAgent(new ServerComunicationProtocolHttpAdapter(id)(using ec), player, 50)
      serverAddress.foreach(playerAgent.registerToServer(_))
      playerAgent.start()

      given ActorSystem[Any] = ActorSystem(Behaviors.empty, "system")
      DomoticASWDeviceHttpInterface("0.0.0.0", port, playerAgent)
