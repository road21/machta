package hkd.crud

import hkd.core
import hkd.core.{Collection, Validatable}

enum UpdateField[+A]:
  case Set(a: A) extends UpdateField[A]
  case Ignore extends UpdateField[Nothing]

final case class ModifyCol[+A](
  add: Vector[A] = Vector(),
  delete: Vector[A] = Vector()
)

trait UpdateCollection[C]:
  type E
  given col: Collection.Aux[C, E]

  def upd: ModifyCol[E]
end UpdateCollection

object UpdateCollection:
  def apply[C] = (col: Collection[C]) ?=> new UpdateCFieldApply[C, col.E]

  final class UpdateCFieldApply[C, A](private val dummy: Boolean = true) extends AnyVal:
    def apply(add: Vector[A] = Vector(), delete: Vector[A] = Vector())(using C: Collection.Aux[C, A]): UpdateCollection[C] =
      new UpdateCollection[C] {
        type E = A
        given col: Collection.Aux[C, E] = summon
        val upd = ModifyCol[E](add, delete)
      }
end UpdateCollection

trait Raw[F[_], Valid]:
  type R

  given valid: Validatable.Aux[F, Valid, R]
  def value: R
  def validate: F[Valid] = valid.validate(value)
end Raw

object Raw:
  def apply[V] = [F[_]] => (vld: Validatable[F, V]) ?=> (v: vld.Raw) =>
    new Raw[F, V] {
      type R = vld.Raw
      given valid: Validatable.Aux[F, V, R] = summon
      val value = v
    }

  implicit def collection[F[_], C, E, R](implicit C: Collection.Aux[C, E], V: Validatable.Aux[F, E, R]): Collection.Aux[Raw[F, C], R] =
    new Collection[Raw[F, C]] { type E = R }
end Raw