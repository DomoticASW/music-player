package domain

import domain.MusicPlayer.MusicPlayerOpsImpl.step
import sleeper.SleeperImpl.sleep
import state.given
import domain.MusicPlayer.MusicPlayerOpsImpl.executeAction
import utils.OneOf
import OneOf.*
import logger.LoggerImpl
import sleeper.SleeperImpl
import domain.MusicPlayer.MusicPlayerOpsImpl.currentState
import domain.MusicPlayer.MusicPlayerOpsImpl.toMs
import ports.ServerComunicationProtocol.*

/** @param musicPlayer
  *   The music player
  * @param periodMs
  *   It is suggested to choose a period which is less than the MEST (Minimum
  *   Event Separation Time)
  */
class MusicPlayerAgent(
    serverProtocol: ServerComunicationProtocol,
    private var _musicPlayer: MusicPlayer,
    periodMs: Long,
    announceEveryMs: Long
) extends Thread:

  def musicPlayer = _musicPlayer
  def musicPlayer_=(m: MusicPlayer) = _musicPlayer = m

  private var actions: Seq[Action] = Seq()

  def enqueAction(e: Action): Unit =
    synchronized:
      actions = actions :+ e

  private def takeActions(): Seq[Action] =
    synchronized:
      val res = actions
      actions = Seq()
      res

  private var serverAddress: Option[ServerAddress] = None

  /** Once registered to a server the agent will send it's state once every
    * `periodMs`
    */
  def registerToServer(serverAddress: ServerAddress): Unit =
    synchronized:
      this.serverAddress = Some(serverAddress)

  def unregister(): Unit =
    synchronized:
      this.serverAddress = None

  private var _shouldStop = false
  private def shouldStop: Boolean = synchronized { _shouldStop }
  def setShouldStop(): Unit = synchronized { _shouldStop = true }

  private var timePassed: Long = 0
  private var timeFromLastAnnounceMs: Long = 0

  override def run(): Unit =
    import Action.*
    while !shouldStop do
      Thread.sleep(periodMs)
      val actions = takeActions()
      val state = actions match
        case h :: t => executeAction(h)
        case Nil    => step(periodMs.toMs)
      val (newState, e) = state.run(musicPlayer.initialState)
      musicPlayer = musicPlayer.withNewState(newState)
      e match
        case Left(event) =>
          this.serverAddress.foreach(this.serverProtocol.sendEvent(_, event))
        case Right(_) => ()

      serverAddress match
        case Some(serverAddress) =>
          if timePassed >= musicPlayer.updateRate
          then
            timePassed = 0
            this.serverProtocol.updateState(serverAddress, newState.s)
          else timePassed = timePassed + periodMs
        case None if timeFromLastAnnounceMs >= announceEveryMs =>
          serverProtocol.announce()
          timeFromLastAnnounceMs = 0
        case None =>
          timeFromLastAnnounceMs += periodMs
