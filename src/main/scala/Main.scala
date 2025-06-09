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

val backInBlack = Music("Back In Black", 100)
val dontStopBelievin = Music("Don't Stop Believin", 50)
val pokersFace = Music("Poker's Face", 70)

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

def emitEvent(e: Either[Event, Unit], ms: Int = 1000) =
  e match
    case Left(event) => 
      println("\nevent emitted: ".red + event + "\n")
      Thread.sleep(ms)
    case _ => ()

def playMusic(): State[MusicState, Unit] =
  for
    e <- step(10)
    _ = emitEvent(e)
    m <- currentState
    _ <- m match
      case Playing(music, t) =>
        println(music.name + " is at " + t + "/" + music.duration)
        val p = Random().nextDouble()
        for
          e <- 
            if p > 0.8
            then pause()
            else
              Thread.sleep(1000)
              play()
          _ = emitEvent(e)
          _ <- playMusic()
        yield e
      case Paused(music, t) =>
        if t >= music.duration then
          Thread.sleep(1000)
          println(music.name + " just finished!\n")
          Thread.sleep(500)
          for
            _ <- changeMusic(if music == backInBlack then dontStopBelievin else if music == dontStopBelievin then pokersFace else backInBlack)
            p = Random().nextDouble()
            e <-
              if p > 0.9
              then stop()
              else play()
            _ = emitEvent(e)
            _ <- playMusic()
          yield e
        else
          println(music.name + " has beed paused")
          println("Restarting in 3 seconds")
          Thread.sleep(3000)
          println(music.name + " is now restarting!")
          Thread.sleep(500)
          for
            e <- play()
            _ = emitEvent(e)
            _ <- playMusic()
          yield e
      case _ => 
        println("Music player powering off")
        stop()
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
    _ <- playMusic()
  yield ()

  state.run(initialState)