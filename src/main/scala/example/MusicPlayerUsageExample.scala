package example

import state.State
import state.given
import utils.given
import utils.Lifter.*
import domain.MusicPlayerOpsImpl.*
import domain.MusicPlayerState.*
import log.LoggerImpl.*
import domain.Event
import domain.Music
import scala.util.Random
import domain.MusicPlayer


type MusicLoggerState = (MusicState, LoggerState)

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def emitEventIfPresent(e: Either[Event, Unit], ms: Int = 1000): State[LoggerState, Unit] =
  for 
    emitted <- emitEvent(e)
    _ = if emitted then Thread.sleep(ms)
  yield ()

def pauseOrContinue(music: Music, t: Int, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " is at " + t + "/" + music.duration).autoLift
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToPause
      then pause().autoLift
      else
        for
          e <- play().autoLift
        yield e
    _ <- emitEventIfPresent(e).autoLift
    _ = Thread.sleep(1000)
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def changeMusicAndLog(music: Music, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " just finished!\n").autoLift
    _ = Thread.sleep(500)
    p = Random().between(0, musics.size)
    e <- changeMusic(musics(p)).autoLift
    _ <- emitEventIfPresent(Left(e)).autoLift
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield Left(e)

def restartingMusic(music: Music, musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " has beed paused").autoLift
    _ <- log("Restarting in 3 seconds").autoLift
    _ = Thread.sleep(3000)
    _ <- log(music.name + " is now restarting!").autoLift
    _ = Thread.sleep(500)
    e <- play().autoLift
    _ <- emitEventIfPresent(e).autoLift
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def startMusic(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double) =
  for
    _ <- currentState.autoLift
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToStop
      then stop().autoLift
      else play().autoLift
    _ <- emitEventIfPresent(e).autoLift
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
  yield e

def playMusic(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double): State[MusicLoggerState, Unit] =
  for
    e <- step(steps).autoLift
    _ <- emitEventIfPresent(e).autoLift
    m <- currentState.autoLift
    _ <- m match
      case Playing(music, t) =>
        pauseOrContinue(music, t, musics, steps, probabilityToPause, probabilityToStop)
      case Paused(music, t) =>
        if t >= music.duration then
          changeMusicAndLog(music, musics, steps, probabilityToPause, probabilityToStop)
        else if t > 0 
          then restartingMusic(music, musics, steps, probabilityToPause, probabilityToStop)
          else startMusic(musics, steps, probabilityToPause, probabilityToStop)
      case _ =>
        for
          _ <- log("Music player powering off").autoLift
          e <- stop().autoLift
        yield e
  yield ()

def startPlayer(musics: Seq[Music], steps: Int, probabilityToPause: Double, probabilityToStop: Double) =
  for
    event <- changeMusic(musics.head).autoLift
    _ <- emitEventIfPresent(Left(event)).autoLift
    e <- play().autoLift
    _ <- emitEventIfPresent(e).autoLift
    _ <- playMusic(musics, steps, probabilityToPause, probabilityToStop)
    events <- events.autoLift
  yield events