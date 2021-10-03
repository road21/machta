package machta

import cats.{Applicative, Traverse}

trait Raw[F[_], Valid]:
  type R

  def valid: Validatable.Aux[F, Valid, R]
  def value: R

  def validate: F[Valid] = valid.validate(value)

object Raw:
  type Aux[F[_], V, r] = Raw[F, V] { type R = r }

  final case class Impl[F[_], v, r](value: r)(using V: Validatable.Aux[F, v, r]) extends Raw[F, v] {
    type R = r
    def valid = V
  }

  def apply[V] = [F[_]] => (vld: Validatable[F, V]) ?=> (v: vld.Raw) => Impl(v)