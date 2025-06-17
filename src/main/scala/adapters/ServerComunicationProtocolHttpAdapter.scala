package adapters

import scala.concurrent.ExecutionContext
import ports.ServerComunicationProtocol.ServerComunicationProtocol
import domain.Event
import scala.concurrent.Future
import domain.MusicPlayer.MusicPlayerOpsImpl.MusicState
import ports.ServerComunicationProtocol.ServerAddress

class ServerComunicationProtocolHttpAdapter(using ExecutionContext) extends ServerComunicationProtocol:
  override def sendEvent(address: ServerAddress, e: Event): Future[Unit] = ???
  override def updateState(address: ServerAddress, state: MusicState): Future[Unit] = ???
