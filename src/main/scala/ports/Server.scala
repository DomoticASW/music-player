package ports

import domain.MusicPlayer.MusicPlayerOpsImpl.MusicState
import scala.concurrent.Future
import domain.Event

trait Server:
  def updateState(state: MusicState): Future[Unit]
  def sendEvent(e: Event): Future[Unit]
