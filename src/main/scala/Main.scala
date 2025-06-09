import domain.Music
import domain.MusicPlayer
import example.startPlayer
import log.LoggerImpl
import utils.OneOf.*
import domain.GlobalState

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
  val initialState: GlobalState = More(domain.MusicPlayerOpsImpl.initialState, More(log.LoggerImpl.initialState, One(sleep.SleeperImpl.initialState)))

  val run: Runnable = () => state.run(initialState)
  val musicPlayerThread = new Thread(run, "musicPlayer")
  musicPlayerThread.start()
  musicPlayerThread.join()