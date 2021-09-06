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

trait Raw[Valid]:
  type R
  type F[_]

  given valid: Validatable.Aux[F, Valid, R]
  def value: R
end Raw

object Raw:
  def apply[V] = [f[_]] => (vld: Validatable[f, V]) ?=> (v: vld.Raw) =>
    new Raw[V] {
      type R = vld.Raw
      type F[x] = f[x]
      given valid: Validatable.Aux[F, V, R] = summon
      val value = v
    }

  implicit def collection[F[_], C, E, R](implicit C: Collection.Aux[C, E], V: Validatable.Aux[F, E, R]): Collection.Aux[Raw[C], R] =
    new Collection[Raw[C]] { type E = R }
end Raw