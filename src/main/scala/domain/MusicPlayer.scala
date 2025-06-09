package domain

import state.State
import state.given
import domain.MusicPlayerOpsImpl.initialState

enum MusicPlayerState:
  case Playing(m: Music, currentTime: Int)
  case Paused(m: Music, currentTime: Int)
  case Off

enum Event:
  case ChangeMusic
  case Resume
  case Pause
  case End

trait MusicPlayer:
  def name: String
  def musics: Set[Music]
  def state: MusicPlayerOpsImpl.MusicState
  def changeState(state: MusicPlayerOpsImpl.MusicState): MusicPlayer

object MusicPlayer:
  case class MusicPlayerImpl(name: String, musics: Set[Music], state: MusicPlayerOpsImpl.MusicState = initialState) extends MusicPlayer:
    override def changeState(state: MusicPlayerOpsImpl.MusicState): MusicPlayer =
      this.copy(state = state)

  def apply(name: String, musics: Set[Music]): MusicPlayer = MusicPlayerImpl(name = name, musics = musics)

trait MusicPlayerOps:
  type MusicState
  def initialState: MusicState
  def log(msg: String): State[MusicState, Unit]
  def currentState: State[MusicState, MusicPlayerState]
  def changeMusic(music: Music): State[MusicState, Event]
  def play(): State[MusicState, Either[Event, Unit]]
  def pause(): State[MusicState, Either[Event, Unit]]
  def stop(): State[MusicState, Either[Event, Unit]]
  def step(seconds: Int): State[MusicState, Either[Event, Unit]]

object MusicPlayerOpsImpl extends MusicPlayerOps:
  import MusicPlayerState.*
  import Event.*

  override opaque type MusicState = MusicPlayerState
  override def initialState: MusicState = Off

  override def currentState: State[MusicState, MusicPlayerState] =
    State(s => (s, s))

  override def log(msg: String): State[MusicState, Unit] =
    println(msg)
    State(s => (s, ()))

  override def changeMusic(music: Music): State[MusicState, Event] =
    State(s => (Playing(music, 0), ChangeMusic))

  override def play(): State[MusicState, Either[Event, Unit]] =
    State(s => s match
      case Paused(m, t) => (Playing(m, t), Left(Resume))
      case _ => (s, Right(()))
    )

  override def pause(): State[MusicState, Either[Event, Unit]] =
    State(s => s match
      case Playing(m, t) => (Paused(m, t), Left(Pause))
      case _ => (s, Right(()))
    )

  override def stop(): State[MusicState, Either[Event, Unit]] =
    State(s => s match
      case Off => (Off, Right(()))
      case _ => (Off, Left(End))
    )

  override def step(seconds: Int): State[MusicState, Either[Event, Unit]] =
    State(s => s match
      case Playing(m, t) =>
        if t + seconds >= m.duration then
          (Paused(m, m.duration), Left(End))
        else
          (Playing(m, t + seconds), Right(()))
      case _ => (s, Right(()))
    )
