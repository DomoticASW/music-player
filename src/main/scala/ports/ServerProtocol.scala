package ports

import domain.MusicPlayer.MusicPlayerOpsImpl.MusicState
import scala.concurrent.Future
import domain.Event
import domain.MusicPlayer

object ServerComunicationProtocol:
  case class ServerAddress(host: String, port: Int)

  trait ServerComunicationProtocol:
    def updateState(address: ServerAddress, state: MusicState): Future[Unit]
    def sendEvent(address: ServerAddress, e: Event): Future[Unit]
    def announce(): Unit
