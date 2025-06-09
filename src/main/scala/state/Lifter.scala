package state

import domain.MusicPlayerOpsImpl.MusicState
import log.LoggerImpl.LoggerState

trait Lifter[Small, Big]:
  def lift[A](sa: State[Small, A]): State[Big, A]

given liftMusic[S2]: Lifter[MusicState, (MusicState, S2)] with
  def lift[A](sa: State[MusicState, A]): State[(MusicState, S2), A] =
    State { case (s1, s2) =>
      val (ns1, a) = sa.run(s1)
      ((ns1, s2), a)
    }

given liftLogger[S1]: Lifter[LoggerState, (S1, LoggerState)] with
  def lift[A](sa: State[LoggerState, A]): State[(S1, LoggerState), A] =
    State { case (s1, s2) =>
      val (ns2, a) = sa.run(s2)
      ((s1, ns2), a)
    }

extension [Small, Big](sa: State[Small, ?])
  def autoLift(using Lifter[Small, Big]): State[Big, ?] =
    summon[Lifter[Small, Big]].lift(sa)
