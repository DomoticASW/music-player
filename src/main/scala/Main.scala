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
  def parse(envVar: String)(default: String): Right[Nothing, String] =
    Right(sys.env.getOrElse(envVar, default))

  def musics: Either[String, Set[Music]] =
    for
      musicsStr <- Right(
        sys.env.get("MUSICS").map(_.split(",").map(_.trim()).toSet)
      )
      musics <- musicsStr match
        case None =>
          Right(
            Set(
              Music("Back In Black", 10),
              Music("Don't Stop Believin", 5),
              Music("Poker's Face", 7)
            )
          )
        case Some(value) if value.size > 1 =>
          val pairs = value.map(_.split("-"))
          if pairs.find(_.length != 2).isDefined || pairs
              .find(!_.apply(1).toIntOption.isDefined)
              .isDefined
          then
            Left(
              "The Musics are defined by the name and the duration (integer) separeted by a -"
            )
          else Right(pairs.map(p => Music(p(0), p(1).toInt)).toSet)
        case _ => Left("At least one music should be given")
    yield musics

  def updateRate: Either[String, Long] =
    for
      updateRateStr <- Right(sys.env.get("UPDATE_RATE"))
      updateRate <- updateRateStr match
        case None => Right(1000L)
        case Some(value) =>
          value.toLongOption.toRight("Update rate should be an integer")
    yield updateRate

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

  def serverDiscoveryPort(default: Int): Either[String, Int] =
    val envVar = "SERVER_DISCOVERY_PORT"
    for
      str <- sys.env.get(envVar) match
        case None        => Right(default.toString())
        case Some(value) => Right(value)
      port <- str.toIntOption match
        case None => Left(s"Invalid port $str is not an integer")
        case Some(p) if p >= 0 & p <= 65335 => Right(p)
        case Some(p) => Left(s"Invalid port $p is out of valid port range")
    yield (port)

  def lanHostname: Either[String, String] =
    for
      lanHostnameStr <- Right(sys.env.get("LAN_HOSTNAME"))
      lanHostname <- lanHostnameStr match
        case None => Left("LAN_HOSTNAME is missing")
        case Some(value) =>
          Right(value)
    yield lanHostname

  val config = for
    id <- parse("ID")(default = "music-player")
    name <- parse("NAME")(default = "Music player")
    m <- musics
    ur <- updateRate
    port <- port(default = 8080)
    serverAddress <- serverAddress(default = None)
    serverDiscoveryPort <- serverDiscoveryPort(default = 30000)
    discoveryBroadcastAddress <- parse("DISCOVERY_BROADCAST_ADDR")(default =
      "255.255.255.255"
    )
    lanHostname <- lanHostname
    config <- ConfigChecker(id, name, m, ur).left.map(_.message)
  yield (
    config,
    port,
    serverAddress,
    serverDiscoveryPort,
    discoveryBroadcastAddress,
    lanHostname
  )

  config match
    case Left(err: String) =>
      Console.err.println(err)
      sys.exit(1)
    case Right(
          (
            config,
            port,
            serverAddress,
            serverDiscoveryPort,
            discoveryBroadcastAddress,
            lanHostname
          )
        ) =>
      val id = config.id
      val name = config.name
      val m = config.musics
      val ur = config.updateRate

      val ec = ExecutionContext.global
      val player = MusicPlayer(id, name, m, ur)
      val playerAgent = MusicPlayerAgent(
        new ServerComunicationProtocolHttpAdapter(
          id,
          name,
          clientPort = port,
          announcePort = serverDiscoveryPort,
          discoveryBroadcastAddress = discoveryBroadcastAddress,
          lanHostname = lanHostname
        )(using ec),
        player,
        periodMs = 50,
        announceEveryMs = 5000
      )
      serverAddress.foreach(playerAgent.registerToServer(_))
      playerAgent.start()

      given ActorSystem[Any] = ActorSystem(Behaviors.empty, "system")
      DomoticASWDeviceHttpInterface("0.0.0.0", port, playerAgent)
