package machta

import cats.{Applicative, Traverse}

trait Raw[F[_], Valid]:
  type R

  def valid: Validatable.Aux[F, Valid, R]
  def value: R

  def validate: F[Valid] = valid.validate(value)
end Raw

object Raw:
  def apply[V] = [F[_]] => (vld: Validatable[F, V]) ?=> (v: vld.Raw) =>
    new Raw[F, V] {
      type R = vld.Raw
      val valid: Validatable.Aux[F, V, R] = summon
      val value = v
    }
end Raw