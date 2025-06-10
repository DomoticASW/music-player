package example

import state.State
import state.given
import utils.given
import utils.*
import OneOf.*
import domain.MusicPlayerOpsImpl.*
import domain.Action.*
import domain.MusicPlayerState.*
import logger.LoggerImpl
import logger.LoggerImpl.*
import domain.Event
import domain.Music
import scala.util.Random
import domain.MusicPlayer
import domain.GlobalState
import sleeper.SleeperImpl
import SleeperImpl.sleep
import SleeperImpl.timePassed

def secondsToMs(seconds: Int) = seconds * 1000

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def pauseOrContinue(music: Music, t: Int, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " is at " + t + "/" + music.duration)
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToPause
      then executeAction(Pause)
      else executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(steps))
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def changeMusicAndLog(music: Music, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " just finished!\n")
    _ <- sleep(500)
    p = Random().between(0, musics.size)
    e <- executeAction(ChangeMusic(musics(p)))
    _ <- emitEvent(e)
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def restartingMusic(music: Music, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " has beed paused")
    _ <- log("Restarting in 3 seconds")
    _ <- sleep(secondsToMs(3))
    _ <- log(music.name + " is now restarting!")
    e <- executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(steps))
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def turnOff(): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log("Music player powering off")
    e <- executeAction(Stop)
  yield e

def startMusic(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- currentState
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToStop
      then executeAction(Stop)
      else executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(steps))
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def playMusic(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Unit] =
  for
    e <- step(steps)
    _ <- emitEvent(e)
    m <- currentState
    _ <- m match
      case Playing(music, t) =>
        pauseOrContinue(music, t, musics, steps, probabilityToPause, probabilityToStop)
      case Paused(music, t) =>
        if t >= music.duration then
          changeMusicAndLog(music, musics, steps, probabilityToPause, probabilityToStop)
        else if t > 0 
          then restartingMusic(music, musics, steps, probabilityToPause, probabilityToStop)
          else startMusic(musics, steps, probabilityToPause, probabilityToStop)
      case _ => turnOff()
  yield ()

def startPlayer(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Unit] =
  for
    event <- executeAction(ChangeMusic(musics.head))
    _ <- emitEvent(event)
    e <- executeAction(Play)
    _ <- emitEvent(e)
    _ <- sleep(secondsToMs(steps))
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
    events <- events
    _ <- log("Events emitted: " + events.mkString(", "))
    timePassed <- timePassed
    _ <- log("Time passed: " + timePassed)
  yield ()