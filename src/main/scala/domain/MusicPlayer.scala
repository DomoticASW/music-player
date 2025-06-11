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

enum MusicPlayerState:
  case Playing(m: Music, currentTime: Int)
  case Paused(m: Music, currentTime: Int)
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
  case ChangeTime(t: Int)
  case Stop

trait MusicPlayer:
  def name: String
  def steps: Int
  def musics: Set[Music]
  def initialState: GlobalState
  def withNewState(m: GlobalState): MusicPlayer

object MusicPlayer:
  private case class MusicPlayerImpl(name: String, musics: Set[Music], steps: Int) extends MusicPlayer:
    var initialState: GlobalState = MusicPlayerOpsImpl.initialState(musics.head)

    override def withNewState(m: GlobalState): MusicPlayer = 
      val copy = this.copy()
      copy.initialState = m
      copy

  def apply(name: String, musics: Set[Music], steps: Int): MusicPlayer =
    MusicPlayerImpl(name = name, musics = musics, steps = steps)

  trait MusicPlayerOps:
    type GlobalState
    def currentState: State[GlobalState, MusicPlayerState]
    def executeAction(action: Action): State[GlobalState, Either[Event, Unit]]
    def step(seconds: Int): State[GlobalState, Either[Event, Unit]]

  object MusicPlayerOpsImpl extends MusicPlayerOps:
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
          case ChangeTime(t) => changeTime(s, t)
          case Stop => stop(s)
      )

    private def play(s: MusicPlayerState) = s match
      case Paused(m, t) => (Playing(m, t), Left(Event.Resume))
      case Off(m) => (Playing(m, 0), Left(Event.Start))
      case _ => (s, Right(()))

    private def pause(s: MusicPlayerState) = s match
      case Playing(m, t) => (Paused(m, t), Left(Event.Pause))
      case _ => (s, Right(()))

    private def changeMusic(s: MusicPlayerState, m: Music) = s match
      case Playing(_, _) => (Playing(m, 0), Left(Event.ChangeMusic))
      case Paused(_, _) => (Paused(m, 0), Left(Event.ChangeMusic))
      case Off(_) => (Off(m), Left(Event.ChangeMusic))

    private def changeTime(s: MusicPlayerState, t: Int) = s match
      case Playing(m, _) => (Playing(m, t), Right(()))
      case Paused(m, _) => (Paused(m, t), Right(()))
      case _ => (s, Right(()))

    private def stop(s: MusicPlayerState) = s match
      case Playing(m, _) => (Off(m), Left(Event.End))
      case Paused(m, _) => (Off(m), Left(Event.End))
      case _ => (s, Right(()))
    
    override def step(ms: Int): State[GlobalState, Either[Event, Unit]] =
      State[MusicState, Either[Event, Unit]](s => 
        s match
          case Playing(m, t) =>
            if t + ms >= m.duration * 1000 then
              (Paused(m, m.duration), Left(Event.End))
            else
              (Playing(m, t + ms), Right(()))
          case _ => (s, Right(()))
      )
