package log

import state.State
import domain.Event
import utils.Lifter
import domain.GlobalState
import utils.given

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

trait Logger:
  type LoggerState
  def initialState: LoggerState
  def log(msg: String): State[GlobalState, Unit]
  def emitEvent(e: Either[Event, Unit]): State[GlobalState, Boolean]
  def events: State[GlobalState, Seq[Event]]

object LoggerImpl extends Logger:
  override opaque type LoggerState = Seq[Event]
  override def initialState: LoggerState = Seq()
  override def emitEvent(e: Either[Event, Unit]): State[GlobalState, Boolean] =
    e match
      case Left(event) =>
        println("\nevent emitted: ".red + event + "\n")
        State[LoggerState, Boolean](s => (s :+ event, true))
      case _ => State[LoggerState, Boolean](s => (s, false))
  override def log(msg: String): State[GlobalState, Unit] =
    println(msg)
    State[LoggerState, Unit](s => (s, ()))
  override def events: State[GlobalState, Seq[Event]] =
    State[LoggerState, Seq[Event]](s => (s, s))