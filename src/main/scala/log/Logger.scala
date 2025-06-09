package log

import state.State
import domain.Event

implicit class ColorString(val str: String) extends AnyVal:
  import scala.Console._
  def red = s"$RED$str$RESET"

trait Logger:
  type LoggerState
  def initialState: LoggerState
  def log(msg: String): State[LoggerState, Unit]
  def emitEvent(e: Either[Event, Unit]): State[LoggerState, Boolean]
  def events: State[LoggerState, Seq[Event]]

object LoggerImpl extends Logger:
  override opaque type LoggerState = Seq[Event]
  override def initialState: LoggerState = Seq()
  override def emitEvent(e: Either[Event, Unit]): State[LoggerState, Boolean] =
    e match
      case Left(event) =>
        println("\nevent emitted: ".red + event + "\n")
        State(s => (s :+ event, true))
      case _ => State(s => (s, false))
  override def log(msg: String): State[LoggerState, Unit] =
    println(msg)
    State(s => (s, ()))
  override def events: State[LoggerState, Seq[Event]] =
    State(s => (s, s))