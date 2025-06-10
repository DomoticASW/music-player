import domain.Music
import domain.MusicPlayer
import example.startPlayer
import logger.* 
import sleeper.*
import domain.*
import utils.OneOf.*

def musics = Set(
  Music("Back In Black", 100),
  Music("Don't Stop Believin", 50),
  Music("Poker's Face", 70)
)

def steps = 1
def probabilityToStop = 0.1
def probabilityToPause = 0.2

def musicPlayerName = "MusicPlayer"

@main
def main =
  val musicPlayer = MusicPlayer(musicPlayerName, musics)

  val state = startPlayer(musics.toSeq, steps, probabilityToPause, probabilityToStop)
  val initialState: GlobalState = More(MusicPlayerOpsImpl.initialState, More(LoggerImpl.initialState, One(SleeperImpl.initialState)))

  val run: Runnable = () => state.run(initialState)
  val musicPlayerThread = new Thread(run, musicPlayerName)
  musicPlayerThread.start()
