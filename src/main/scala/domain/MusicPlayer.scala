package domain

import state.State

private[domain] object MusicPlayer:
  enum MusicPlayerState:
    case Playing(m: Music)
    case Paused(m: Music)

  enum Event:
    case ChangeMusic(m: Music)
    case Resume(m: Music)
    case Pause(m: Music)
    case End(m: Music)

  trait Music:
    type MusicState
    def name: String
    def duration: Int
    def currentState: State[MusicState, Int]
    def step(seconds: Int): State[MusicState, Unit]

  private case class MusicImpl(override val name: String, override val duration: Int) extends Music:
    override opaque type MusicState = Int
    override def currentState: State[MusicState, Int] =
      State(s => (s, s))
    override def step(seconds: Int): State[MusicState, Unit] =
      State(s =>
        s match
          case t if t + seconds >= duration => (duration, ())
          case _ => (s + seconds, ())
      )

trait MusicPlayer:
  import domain.MusicPlayer.*
  type MusicState
  def name: String
  def musics: Set[Music]
  def currentState: State[MusicState, MusicPlayerState]
  def changeMusic(music: Music): State[MusicState, Event]
  def play(): State[MusicState, Event]
  def pause(): State[MusicState, Event]
  def step(seconds: Int): State[MusicState, Either[Event, Unit]]
