import domain.Music
import domain.MusicPlayer
import example.startPlayer
import domain.MusicPlayerOpsImpl.initialState
import log.LoggerImpl

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

@main
def main =
  val musicPlayer = MusicPlayer("MyMusicPlayer", musics)

  val state = startPlayer(musics.toSeq, steps, probabilityToPause, probabilityToStop)

  val run: Runnable = () => println(state.run((initialState, LoggerImpl.initialState))._2)
  val musicPlayerThread = new Thread(run, "musicPlayer")
  musicPlayerThread.start()
  musicPlayerThread.join()