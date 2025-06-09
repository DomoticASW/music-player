import domain.*
import MusicPlayerOpsImpl.changeMusic
import state.given
import domain.MusicPlayerOpsImpl.initialState
import domain.MusicPlayerOpsImpl.step
import domain.MusicPlayerOpsImpl.MusicState
import MusicPlayerState. *
import state.State
import domain.MusicPlayerOpsImpl.currentState
import scala.util.Random
import domain.MusicPlayerOpsImpl.pause
import domain.MusicPlayerOpsImpl.play
import domain.MusicPlayerOpsImpl.stop

import scala.Console.{RED, RESET}
import domain.MusicPlayerOpsImpl.log

val backInBlack = Music("Back In Black", 100)
val dontStopBelievin = Music("Don't Stop Believin", 50)
val pokersFace = Music("Poker's Face", 70)

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def emitEvent(e: Either[Event, Unit], ms: Int = 1000) =
  e match
    case Left(event) =>
      for 
        _ <- log("\nevent emitted: ".red + event + "\n")
        _ = Thread.sleep(ms)
      yield ()
    case _ => ()

def pauseOrContinue(music: Music, t: Int, steps: Int): State[MusicState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " is at " + t + "/" + music.duration)
    p = Random().nextDouble()
    e <-
      if p > 0.8
      then pause()
      else
        for
          e <- play()
        yield e
    _ = emitEvent(e)
    _ = Thread.sleep(1000)
    _ <- playMusic(steps)
  yield e

def changeMusicOrStopPlayer(music: Music, steps: Int): State[MusicState, Either[Event, Unit]] =
  for
    _ <- log(music.name + " just finished!\n")
    _ = Thread.sleep(500)
    _ <- changeMusic(if music == backInBlack then dontStopBelievin else if music == dontStopBelievin then pokersFace else backInBlack)
    p = Random().nextDouble()
    e <-
      if p > 0.9
      then stop()
      else play()
    _ = emitEvent(e)
    _ <- playMusic(steps)
  yield e

def restartingMusic(music: Music, steps: Int) =
  for
    _ <- log(music.name + " has beed paused")
    _ <- log("Restarting in 3 seconds")
    _ = Thread.sleep(3000)
    _ <- log(music.name + " is now restarting!")
    _ = Thread.sleep(500)
    e <- play()
    _ = emitEvent(e)
    _ <- playMusic(steps)
  yield e

def playMusic(steps: Int): State[MusicState, Unit] =
  for
    e <- step(steps)
    _ = emitEvent(e)
    m <- currentState
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
          _ <- log("Music player powering off")
          e <- stop()
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

  val state = for
    event <- changeMusic(backInBlack)
    _ = emitEvent(Left(event))
    _ <- playMusic(steps = 10)
  yield ()

  val run: Runnable = () => state.run(initialState)
  val musicPlayerThread = new Thread(run, "musicPlayer")
  musicPlayerThread.start()
  musicPlayerThread.join()