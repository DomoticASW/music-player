package domain

import domain.MusicPlayer.MusicPlayerOpsImpl.step
import sleeper.SleeperImpl.sleep
import state.given
import domain.MusicPlayer.MusicPlayerOpsImpl.executeAction
import utils.OneOf
import OneOf.*
import logger.LoggerImpl
import sleeper.SleeperImpl

/** @param musicPlayer
  *   The music player state
  * @param periodMs
  *   It is suggested to choose a period which is less than the MEST (Minimum
  *   Event Separation Time)
  */
class MusicPlayerAgent(private val musicPlayer: MusicPlayer, periodMs: Long) extends Thread:

  val steps = musicPlayer.steps

  var _previousState: GlobalState = More(musicPlayer.initialState, More(LoggerImpl.initialState, One(SleeperImpl.initialState)))

  def previousState: GlobalState = _previousState
  def previousState_=(newState: GlobalState) = _previousState = newState

  private var actions: Seq[Action] = Seq()

  def enqueAction(e: Action): Unit =
    synchronized:
      actions = actions :+ e

  private def takeActions(): Seq[Action] =
    synchronized:
      val res = actions
      actions = Seq()
      res

  private var _shouldStop = false
  private def shouldStop: Boolean = synchronized { _shouldStop }
  def setShouldStop(): Unit = synchronized { _shouldStop = true }

  //TODO: Send state to server when time passed == steps
  //TODO: Send event to server when e is Left(event)
  override def run(): Unit =
    import Action.*
    while !shouldStop do
      Thread.sleep(periodMs)
      val actions = takeActions()
      val state = actions match
        case h :: t => executeAction(h)
        case Nil => step(periodMs.toInt)
      val (newState, e) = state.run(previousState)
      previousState = newState
      e match
        case Left(event) => ()
        case Right(_) => ()