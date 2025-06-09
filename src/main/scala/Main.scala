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

def emitEvent(event: Event, ms: Int = 500) =
  println("event emitted: ".red + event)
  Thread.sleep(ms)

def playMusic(): State[MusicState, Unit] =
  for
    e <- step(10)
    _ = e match
      case Left(event) => emitEvent(event)
      case _ => ()
    m <- currentState
    _ <- m match
      case Playing(music, t) =>
        println(music.name + " is at " + t + "/" + music.duration)
        val p = Random().nextDouble()
        for
          e <- 
            if p > 0.8
            then pause()
            else play()
          _ = e match
            case Left(event) => emitEvent(event)
            case _ => ()
          _ = Thread.sleep(1000)
          _ <- playMusic()
        yield e
      case Paused(music, t) =>
        if t >= music.duration then
          Thread.sleep(1000)
          println(music.name + " just finished!")
          println()
          Thread.sleep(500)
          for
            _ <- changeMusic(if music == backInBlack then dontStopBelievin else if music == dontStopBelievin then pokersFace else backInBlack)
            p = Random().nextDouble()
            e <-
              if p > 0.9
              then stop()
              else play()
            _ = e match
              case Left(event) => emitEvent(event, 1000)
              case _ => ()
            _ <- playMusic()
          yield e
        else
          println(music.name + " has beed paused\n")
          println("Restarting in 3 seconds")
          Thread.sleep(3000)
          println(music.name + " is now restarting!\n")
          Thread.sleep(500)
          for
            e <- play()
            _ = e match
              case Left(event) => emitEvent(event)
              case _ => ()
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
    _ = emitEvent(event)
    _ <- playMusic()
  yield ()

  state.run(initialState)