package domain

import state.State
import state.given
import domain.MusicPlayerOpsImpl.initialState
import utils.Lifter
import utils.given
import utils.OneOf

enum MusicPlayerState:
  case Playing(m: Music, currentTime: Int)
  case Paused(m: Music, currentTime: Int)
  case Off

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
  case Stop

trait MusicPlayer:
  def name: String
  def musics: Set[Music]

object MusicPlayer:
  case class MusicPlayerImpl(name: String, musics: Set[Music]) extends MusicPlayer

  def apply(name: String, musics: Set[Music]): MusicPlayer = MusicPlayerImpl(name = name, musics = musics)

trait MusicPlayerOps:
  type MusicState
  def initialState: MusicState
  def currentState: State[GlobalState, MusicPlayerState]
  def executeAction(action: Action): State[GlobalState, Either[Event, Unit]]
  def step(seconds: Int): State[GlobalState, Either[Event, Unit]]

object MusicPlayerOpsImpl extends MusicPlayerOps:
  import MusicPlayerState.*
  import Event.*

  override opaque type MusicState = MusicPlayerState

  override def initialState: MusicState = Off

  override def currentState: State[GlobalState, MusicPlayerState] =
    State[MusicState, MusicPlayerState](s => (s, s))

  override def executeAction(action: Action): State[GlobalState, Either[Event, Unit]] =
    import Action.*
    State[MusicState, Either[Event, Unit]](s =>
      action match
        case Play => play(s)
        case Pause => pause(s)
        case ChangeMusic(m) => changeMusic(m)
        case Stop => stop(s)
    )

  private def play(s: MusicPlayerState) = s match
    case Paused(m, t) => (Playing(m, t), Left(if t > 0 then Event.Resume else Event.Start))
    case _ => (s, Right(()))

  private def pause(s: MusicPlayerState) = s match
    case Playing(m, t) => (Paused(m, t), Left(Event.Pause))
    case _ => (s, Right(()))

  private def changeMusic(m: Music) =
    (Paused(m, 0), Left(Event.ChangeMusic))

  private def stop(s: MusicPlayerState) = s match
    case Off => (Off, Right(()))
    case _ => (Off, Left(Event.End))
  
  override def step(seconds: Int): State[GlobalState, Either[Event, Unit]] =
    State[MusicState, Either[Event, Unit]](s => 
      s match
        case Playing(m, t) =>
          if t + seconds >= m.duration then
            (Paused(m, m.duration), Left(Event.End))
          else
            (Playing(m, t + seconds), Right(()))
        case _ => (s, Right(()))
    )
