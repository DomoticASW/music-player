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
  def changeMusic(music: Music): State[GlobalState, Event]
  def play(): State[GlobalState, Either[Event, Unit]]
  def pause(): State[GlobalState, Either[Event, Unit]]
  def stop(): State[GlobalState, Either[Event, Unit]]
  def step(seconds: Int): State[GlobalState, Either[Event, Unit]]

object MusicPlayerOpsImpl extends MusicPlayerOps:
  import MusicPlayerState.*
  import Event.*

  override opaque type MusicState = MusicPlayerState

  override def initialState: MusicState = Off

  override def currentState: State[GlobalState, MusicPlayerState] =
    State[MusicState, MusicPlayerState](s => (s, s))

  override def changeMusic(music: Music): State[GlobalState, Event] =
    State[MusicState, Event](s => (Paused(music, 0), ChangeMusic))

  override def play(): State[GlobalState, Either[Event, Unit]] =
    State[MusicState, Either[Event, Unit]](s => s match
      case Paused(m, t) => (Playing(m, t), Left(if t > 0 then Resume else Start))
      case _ => (s, Right(()))
    )

  override def pause(): State[GlobalState, Either[Event, Unit]] =
    State[MusicState, Either[Event, Unit]](s => s match
      case Playing(m, t) => (Paused(m, t), Left(Pause))
      case _ => (s, Right(()))
    )

  override def stop(): State[GlobalState, Either[Event, Unit]] =
    State[MusicState, Either[Event, Unit]](s => s match
      case Off => (Off, Right(()))
      case _ => (Off, Left(End))
    )

  override def step(seconds: Int): State[GlobalState, Either[Event, Unit]] =
    State[MusicState, Either[Event, Unit]](s => s match
      case Playing(m, t) =>
        if t + seconds >= m.duration then
          (Paused(m, m.duration), Left(End))
        else
          (Playing(m, t + seconds), Right(()))
      case _ => (s, Right(()))
    )
