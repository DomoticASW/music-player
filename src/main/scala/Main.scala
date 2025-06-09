import domain.*
import MusicPlayerOpsImpl.changeMusic
import domain.MusicPlayerOpsImpl.initialState
import domain.MusicPlayerOpsImpl.step
import domain.MusicPlayerOpsImpl.MusicState
import MusicPlayerState. *
import domain.MusicPlayerOpsImpl.currentState
import scala.util.Random
import domain.MusicPlayerOpsImpl.pause
import domain.MusicPlayerOpsImpl.play
import domain.MusicPlayerOpsImpl.stop
import log.LoggerImpl
import LoggerImpl.*
import state.*
import state.given
import utils.given
import utils.Lifter
import Lifter.*


val backInBlack = Music("Back In Black", 100)
val dontStopBelievin = Music("Don't Stop Believin", 50)
val pokersFace = Music("Poker's Face", 70)

val musics = Set(
  backInBlack,
  dontStopBelievin,
  pokersFace
)

val steps = 10
val probabilityToStop = 0.1
val probabilityToPause = 0.2

type MusicLoggerState = (MusicState, LoggerState)

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def emitEventIfPresent(e: Either[Event, Unit], ms: Int = 1000): State[LoggerState, Unit] =
  for 
    emitted <- emitEvent(e)
    _ = if emitted then Thread.sleep(ms)
  yield ()

def pauseOrContinue(music: Music, t: Int, steps: Int): State[MusicLoggerState, Either[Event, Unit]] =
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
    _ <- playMusic()
  yield e

def changeMusicAndLog(music: Music, steps: Int): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " just finished!\n").autoLift
    _ = Thread.sleep(500)
    e <- changeMusic(if music == backInBlack then dontStopBelievin else if music == dontStopBelievin then pokersFace else backInBlack).autoLift
    _ <- emitEventIfPresent(Left(e)).autoLift
    _ <- playMusic()
  yield Left(e)

def restartingMusic(music: Music, steps: Int): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " has beed paused").autoLift
    _ <- log("Restarting in 3 seconds").autoLift
    _ = Thread.sleep(3000)
    _ <- log(music.name + " is now restarting!").autoLift
    _ = Thread.sleep(500)
    e <- play().autoLift
    _ <- emitEventIfPresent(e).autoLift
    _ <- playMusic()
  yield e

def startMusic() =
  for
    _ <- currentState.autoLift
    p = Random().nextDouble()
    e <-
      if p > 1 - probabilityToStop
      then stop().autoLift
      else play().autoLift
    _ <- emitEventIfPresent(e).autoLift
    _ <- playMusic()
  yield e

def playMusic(): State[MusicLoggerState, Unit] =
  for
    e <- step(steps).autoLift
    _ <- emitEventIfPresent(e).autoLift
    m <- currentState.autoLift
    _ <- m match
      case Playing(music, t) =>
        pauseOrContinue(music, t, steps)
      case Paused(music, t) =>
        if t >= music.duration then
          changeMusicAndLog(music, steps)
        else if t > 0 
          then restartingMusic(music, steps)
          else startMusic()
      case _ =>
        for
          _ <- log("Music player powering off").autoLift
          e <- stop().autoLift
        yield e
  yield ()

@main
def main =
  val musicPlayer = MusicPlayer("MyMusicPlayer", musics)

  val state = for
    event <- changeMusic(backInBlack).autoLift
    _ <- emitEventIfPresent(Left(event)).autoLift
    e <- play().autoLift
    _ <- emitEventIfPresent(e).autoLift
    _ <- playMusic()
    events <- events.autoLift
  yield events

  val run: Runnable = () => println(state.run((initialState, LoggerImpl.initialState))._2)
  val musicPlayerThread = new Thread(run, "musicPlayer")
  musicPlayerThread.start()
  musicPlayerThread.join()