package sleeper

import state.*
import domain.GlobalState
import utils.given

trait Sleeper:
  type SleeperState
  def initialState: SleeperState
  def wait(ms: Int): State[GlobalState, Unit]
  def timePassed: State[GlobalState, Int]

object SleeperImpl extends Sleeper:
  override opaque type SleeperState = Int
  override def initialState: SleeperState = 0

  override def timePassed: State[GlobalState, Int] =
    State[SleeperState, Int](s => (s, s))

  override def wait(ms: Int): State[GlobalState, Unit] =
    State[SleeperState, Unit](s => { Thread.sleep(ms); (s + ms, ())})