package example

import state.State
import state.given
import utils.given
import utils.*
import OneOf.*
import domain.MusicPlayerOpsImpl.*
import domain.MusicPlayerState.*
import logger.LoggerImpl
import logger.LoggerImpl.*
import domain.Event
import domain.Music
import scala.util.Random
import domain.MusicPlayer
import domain.GlobalState
import sleeper.SleeperImpl.*
import sleeper.SleeperImpl

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def emitEventIfPresent(e: Either[Event, Unit], ms: Int = 1000): State[GlobalState, Unit] =
  for 
    emitted <- emitEvent(e)
    _ <- if emitted then SleeperImpl.wait(500) else SleeperImpl.wait(0)
  yield ()

def pauseOrContinue(music: Music, t: Int, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " is at " + t + "/" + music.duration)
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToPause
      then pause()
      else
        for
          e <- play()
        yield e
    _ <- emitEventIfPresent(e)
    _ <- SleeperImpl.wait(1000)
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def changeMusicAndLog(music: Music, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " just finished!\n")
    _ <- SleeperImpl.wait(500)
    p = Random().between(0, musics.size)
    e <- changeMusic(musics(p))
    _ <- emitEventIfPresent(Left(e))
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield Left(e)

def restartingMusic(music: Music, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " has beed paused")
    _ <- log("Restarting in 3 seconds")
    _ <- SleeperImpl.wait(3000)
    _ <- log(music.name + " is now restarting!")
    _ <- SleeperImpl.wait(500)
    e <- play()
    _ <- emitEventIfPresent(e)
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def turnOff(): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- log("Music player powering off")
    e <- stop()
  yield e

def startMusic(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Either[Event, Unit]] =
  for
    _ <- currentState
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToStop
      then stop()
      else play()
    _ <- emitEventIfPresent(e)
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def playMusic(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[GlobalState, Unit] =
  for
    e <- step(steps)
    _ <- emitEventIfPresent(e)
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
    event <- changeMusic(musics.head)
    _ <- emitEventIfPresent(Left(event))
    e <- play()
    _ <- emitEventIfPresent(e)
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
    events <- events
    _ <- log("Events emitted: " + events.mkString(", "))
    timePassed <- timePassed
    _ <- log("Time passed(ms): " + timePassed)
  yield ()