package hkd.core

trait Validatable[Valid]:
  type Raw

object Validatable:
  type Aux[V, R] = Validatable[V] { type Raw = R }

  implicit def validCollection[F[_], E, R](implicit C: Collection.Aux[F[E], E], V: Validatable.Aux[E, R]): Validatable.Aux[F[E], F[R]] =
    new Validatable[F[E]] { type Raw = F[R] }