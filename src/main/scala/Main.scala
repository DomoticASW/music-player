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

extension [Small, Big, A](sa: State[Small, A])
  def autoLift(using Lifter[Small, Big]): State[Big, A] =
    summon[Lifter[Small, Big]].lift(sa)

type MusicLoggerState = (MusicState, LoggerState)

val backInBlack = Music("Back In Black", 100)
val dontStopBelievin = Music("Don't Stop Believin", 50)
val pokersFace = Music("Poker's Face", 70)

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
      if p > 0.8
      then pause().autoLift
      else
        for
          e <- play().autoLift
        yield e
    _ = emitEventIfPresent(e).autoLift
    _ = Thread.sleep(1000)
    _ <- playMusic(steps)
  yield e

def changeMusicOrStopPlayer(music: Music, steps: Int): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " just finished!\n").autoLift
    _ = Thread.sleep(500)
    _ <- changeMusic(if music == backInBlack then dontStopBelievin else if music == dontStopBelievin then pokersFace else backInBlack).autoLift
    p = Random().nextDouble()
    e <-
      if p > 0.9
      then stop().autoLift
      else play().autoLift
    _ = emitEventIfPresent(e).autoLift
    _ <- playMusic(steps)
  yield e

def restartingMusic(music: Music, steps: Int): State[MusicLoggerState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " has beed paused").autoLift
    _ <- log("Restarting in 3 seconds").autoLift
    _ = Thread.sleep(3000)
    _ <- log(music.name + " is now restarting!").autoLift
    _ = Thread.sleep(500)
    e <- play().autoLift
    _ = emitEventIfPresent(e).autoLift
    _ <- playMusic(steps)
  yield e

def playMusic(steps: Int): State[MusicLoggerState, Unit] =
  for
    e <- step(steps).autoLift
    _ = emitEventIfPresent(e).autoLift
    m <- currentState.autoLift
    _ <- m match
      case Playing(music, t) =>
        pauseOrContinue(music, t, steps)
      case Paused(music, t) =>
        if t >= music.duration then
          changeMusicOrStopPlayer(music, steps)
        else
          restartingMusic(music, steps)
      case _ =>
        for
          _ <- log("Music player powering off").autoLift
          e <- stop().autoLift
        yield e
  yield ()

@main
def main =
  val musics = Set(
    backInBlack,
    dontStopBelievin,
    pokersFace
  )

  val musicPlayer = MusicPlayer("MyMusicPlayer", musics)

  val state: State[MusicLoggerState, Unit] = for
    event <- changeMusic(backInBlack).autoLift
    _ = emitEventIfPresent(Left(event)).autoLift
    _ <- playMusic(steps = 10)
  yield ()

  val run: Runnable = () => state.run((initialState, LoggerImpl.initialState))
  val musicPlayerThread = new Thread(run, "musicPlayer")
  musicPlayerThread.start()
  musicPlayerThread.join()