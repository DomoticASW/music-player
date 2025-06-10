package logger

import state.State
import domain.Event
import utils.Lifter
import domain.GlobalState
import utils.given

import scala.Console.*
extension (str: String)
  def red = s"$RED$str$RESET"
  def green = s"$GREEN$str$RESET"

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
    State[LoggerState, Boolean](s => 
      e match
      case Left(event) =>
        println("event emitted: ".red + event.toString().green)
        (s :+ event, true)
      case _ => (s, false)
    )
  override def log(msg: String): State[GlobalState, Unit] =
    State[LoggerState, Unit](s => 
      println(msg)
      (s, ())
    )
  override def events: State[GlobalState, Seq[Event]] =
    State[LoggerState, Seq[Event]](s => (s, s))