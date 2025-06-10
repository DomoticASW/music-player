package sleeper

import state.*
import domain.GlobalState
import utils.given

trait Sleeper:
  type SleeperState
  def initialState: SleeperState
  def sleep(ms: Long): State[GlobalState, Unit]
  def timePassed: State[GlobalState, Long]

object SleeperImpl extends Sleeper:
  override opaque type SleeperState = Long
  override def initialState: SleeperState = 0

  override def timePassed: State[GlobalState, Long] =
    State[SleeperState, Long](s => (s, s))

  override def sleep(ms: Long): State[GlobalState, Unit] =
    State[SleeperState, Unit](s => { Thread.sleep(ms); (if s + ms >= Long.MaxValue then Long.MaxValue else s + ms, ())})