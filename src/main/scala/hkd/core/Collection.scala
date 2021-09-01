package hkd.core

trait Collection[C]:
  type E


object Collection:
  type Aux[C, El] = Collection[C] { type E = El }

  implicit def seqInstance[A, F[x] <: Seq[x]]: Aux[F[A], A] = new Collection[F[A]] { type E = A }
