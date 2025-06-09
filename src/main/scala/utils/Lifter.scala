package utils

import domain.MusicPlayerOpsImpl.MusicState
import log.LoggerImpl.LoggerState
import state.State

enum OneOf:
  case One[S](s: S)
  case More[S, Tail <: OneOf](s: S, tail: Tail)

trait Lifter[Small, Big <: OneOf]:
  def lift[A](s: State[Small, A]): State[Big, A]

import OneOf.*
given baseLifter[S]: Lifter[S, One[S]] with
  def lift[A](sa: State[S, A]): State[One[S], A] =
    State(one =>
      val (newState, a) = sa.run(one.s)
      (One(newState), a)
    )

given headLifter[S, Tail <: OneOf]: Lifter[S, More[S, Tail]] with
  def lift[A](sa: State[S, A]): State[More[S, Tail], A] =
    State(more =>
      val (newState, a) = sa.run(more.s)
      (More(newState, more.tail), a)
    )

given tailLifter[S, H, Tail <: OneOf](using lt: Lifter[S, Tail]): Lifter[S, More[H, Tail]] with
  def lift[A](sa: State[S, A]): State[More[H, Tail], A] =
    State(more =>
      val (newTail, a) = lt.lift(sa).run(more.tail)
      (More(more.s, newTail), a)
    )

given autoLifter[S, Big <: OneOf, A](using lifter: Lifter[S, Big]): Conversion[State[S, A], State[Big, A]] with
    def apply(sa: State[S, A]): State[Big, A] = lifter.lift(sa)