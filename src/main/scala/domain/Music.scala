package domain

import state.State

trait Music:
  type MusicState
  def name: String
  def duration: Int

object Music:
  private case class MusicImpl(name: String, duration: Int) extends Music

  def apply(name: String, duration: Int): Music = MusicImpl(name, duration)
