package example

import state.State
import state.given
import utils.given
import utils.*
import OneOf.*
import domain.MusicPlayer.MusicPlayerOpsImpl.*
import domain.Action.*
import domain.MusicPlayerState.*
import logger.LoggerImpl
import logger.LoggerImpl.*
import domain.Event
import domain.Music
import scala.util.Random
import domain.MusicPlayer
import sleeper.SleeperImpl
import SleeperImpl.sleep
import SleeperImpl.timePassed

def secondsToMs(seconds: Int) = seconds * 1000

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def pauseOrContinue(music: Music, t: Int, player: MusicPlayer, probabilityToPause: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " is at " + ((t / music.duration.toDouble) * 100).toInt + "%")
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToPause
      then executeAction(Pause)
      else executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(player.steps))
    _ <- playMusic(player, probabilityToPause)
  yield e

def changeMusicAndLog(music: Music, player: MusicPlayer, probabilityToPause: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log("\n" + music.name + " just finished!\n")
    _ <- sleep(500)
    p = Random().between(0, player.musics.size)
    e <- executeAction(ChangeMusic(player.musics.toSeq(p)))
    _ <- emitEvent(e)
    _ <- executeAction(Play)
    _ <- sleep(secondsToMs(player.steps))
    _ <- playMusic(player, probabilityToPause)
  yield e

def restartingMusic(music: Music, player: MusicPlayer, probabilityToPause: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " has beed paused")
    _ <- log("Restarting in 3 seconds")
    _ <- sleep(secondsToMs(3))
    _ <- log(music.name + " is now restarting!")
    e <- executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(player.steps))
    _ <- playMusic(player, probabilityToPause)
  yield e

def turnOff(): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log("Music player powering off")
    e <- executeAction(Stop)
  yield e

def startMusic(player: MusicPlayer, probabilityToPause: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- executeAction(Play)
    _ <- sleep(secondsToMs(player.steps))
    _ <- playMusic(player, probabilityToPause)
  yield Right(())

def playMusic(player: MusicPlayer, probabilityToPause: Double): State[GlobalState, Unit] =
  for
    e <- step(player.steps)
    _ <- emitEvent(e)
    m <- currentState
    p = Random().nextDouble()
    _ <- m match
      case Playing(music, t) =>
        pauseOrContinue(music, t, player, probabilityToPause)
      case Paused(music, t) =>
        if t >= music.duration then
          changeMusicAndLog(music, player, probabilityToPause)
        else
          restartingMusic(music, player, probabilityToPause)
      case _ => startMusic(player, probabilityToPause)
  yield ()

def startPlayer(player: MusicPlayer, probabilityToPause: Double): State[GlobalState, Unit] =
  for
    _ <- log(player.name.toUpperCase())
    _ <- log("Available musics: " + player.musics.map(_.name).mkString(", "))
    _ <- log("\nStarting now!")
    e <- executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(player.steps))
    _ <- playMusic(player, probabilityToPause) //Infinite loop, following lines are just as an example of what can be done
    events <- events
    _ <- log("Events emitted: " + events.mkString(", "))
    timePassed <- timePassed
    _ <- log("Time passed: " + timePassed)
  yield ()