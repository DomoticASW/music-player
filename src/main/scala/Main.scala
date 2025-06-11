import domain.Music
import domain.MusicPlayer
import example.startPlayer
import logger.* 
import sleeper.*
import domain.*
import utils.OneOf.*
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors

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

  def steps: Either[String, Int] =
    for
      stepsStr <- Right(sys.env.get("STEPS"))
      steps <- stepsStr match
        case None => Right(1)
        case Some(value) =>
          value.toIntOption.toRight("Steps should be an integer")
    yield steps

  def probabilityToPause = 0.2

  def musicPlayerName: Either[String, String] = Right(sys.env.get("NAME").getOrElse("Music player"))

  val config = for
    name <- musicPlayerName
    m <- musics
    s <- steps
    config <- ConfigChecker(name, m, s).left.map(_.message)
  yield config

  config match
    case Left(err: String) =>
      Console.err.println(err)
      sys.exit(1)
    case Right(config) =>
      val name = config.name
      val m = config.musics
      val s = config.steps

      val player = MusicPlayer(name, m, s)
      val playerAgent = MusicPlayerAgent(player, 50)
      playerAgent.start()

      given ActorSystem[Any] = ActorSystem(Behaviors.empty, "system")
