package domain

import state.State
import state.given
import utils.Lifter
import utils.given
import utils.OneOf.*
import logger.LoggerImpl
import sleeper.SleeperImpl
import logger.LoggerImpl.LoggerState
import sleeper.SleeperImpl.SleeperState
import domain.MusicPlayer.MusicPlayerOpsImpl.GlobalState
import domain.MusicPlayer.MusicPlayerOpsImpl.Milliseconds
import domain.MusicPlayer.MusicPlayerOpsImpl.Seconds

enum MusicPlayerState:
  case Playing(m: Music, currentTime: Milliseconds)
  case Paused(m: Music, currentTime: Milliseconds)
  case Off(m: Music)

enum Event:
  case ChangeMusic
  case Start
  case Resume
  case Pause
  case End

enum Action:
  case Play
  case Pause
  case ChangeMusic(m: Music)
  case ChangeTime(percentage: Int)
  case Stop

trait MusicPlayer:
  def id: String
  def name: String
  def steps: Long
  def musics: Set[Music]
  def initialState: GlobalState
  def withNewState(m: GlobalState): MusicPlayer

object MusicPlayer:
  private case class MusicPlayerImpl(id: String, name: String, musics: Set[Music], steps: Long) extends MusicPlayer:
    var initialState: GlobalState = MusicPlayerOpsImpl.initialState(musics.head)

    override def withNewState(m: GlobalState): MusicPlayer = 
      val copy = this.copy()
      copy.initialState = m
      copy

  def apply(id: String, name: String, musics: Set[Music], steps: Long): MusicPlayer =
    MusicPlayerImpl(id = id, name = name, musics = musics, steps = steps)

  trait MusicPlayerOps:
    type GlobalState
    def currentState: State[GlobalState, MusicPlayerState]
    def executeAction(action: Action): State[GlobalState, Either[Event, Unit]]
    def step(ms: Milliseconds): State[GlobalState, Either[Event, Unit]]

  object MusicPlayerOpsImpl extends MusicPlayerOps:
    opaque type Seconds = Int
    opaque type Milliseconds = Long

    extension (t: Seconds)
      def toMs: Milliseconds = t * 1000
      def toInt: Int = t

    extension (i: Long)
      def toMs: Milliseconds = i
    
    extension (t: Milliseconds)
      def toSeconds: Seconds = (t / 1000.0).toInt

    import MusicPlayerState.*
    import Event.*

    opaque type MusicState = MusicPlayerState
    override type GlobalState = More[MusicState, More[LoggerState, One[SleeperState]]]

    private[MusicPlayer] def initialState(m: Music): GlobalState = More(Off(m), More(LoggerImpl.initialState, One(SleeperImpl.initialState)))

    override def currentState: State[GlobalState, MusicPlayerState] =
      State[MusicState, MusicPlayerState](s => (s, s))

    override def executeAction(action: Action): State[GlobalState, Either[Event, Unit]] =
      import Action.*
      State[MusicState, Either[Event, Unit]](s =>
        action match
          case Play => play(s)
          case Pause => pause(s)
          case ChangeMusic(m) => changeMusic(s, m)
          case ChangeTime(p) => changeTime(s, p)
          case Stop => stop(s)
      )

    private def play(s: MusicPlayerState) = s match
      case Paused(m, t) => (Playing(m, t), Left(Event.Resume))
      case Off(m) => (Playing(m, 0l), Left(Event.Start))
      case _ => (s, Right(()))

    private def pause(s: MusicPlayerState) = s match
      case Playing(m, t) => (Paused(m, t), Left(Event.Pause))
      case _ => (s, Right(()))

    private def changeMusic(s: MusicPlayerState, m: Music) = s match
      case Playing(_, _) => (Playing(m, 0l), Left(Event.ChangeMusic))
      case Paused(_, _) => (Paused(m, 0l), Left(Event.ChangeMusic))
      case Off(_) => (Off(m), Left(Event.ChangeMusic))

    private def changeTime(s: MusicPlayerState, p: Int) = s match
      case Playing(m, _) => (Playing(m, (p / 100.0 * m.duration.toMs).toLong), Right(()))
      case Paused(m, _) => (Paused(m, (p / 100.0 * m.duration.toMs).toLong), Right(()))
      case _ => (s, Right(()))

    private def stop(s: MusicPlayerState) = s match
      case Playing(m, _) => (Off(m), Left(Event.End))
      case Paused(m, _) => (Off(m), Left(Event.End))
      case _ => (s, Right(()))
    
    override def step(ms: Milliseconds): State[GlobalState, Either[Event, Unit]] =
      State[MusicState, Either[Event, Unit]](s => 
        s match
          case Playing(m, t) =>
            if t + ms >= m.duration.toMs then
              (Paused(m, m.duration.toMs), Left(Event.End))
            else
              (Playing(m, t + ms), Right(()))
          case _ => (s, Right(()))
      )
