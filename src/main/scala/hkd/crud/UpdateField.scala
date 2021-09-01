package hkd.crud

import hkd.core
import hkd.core.{Collection, Validatable}

enum UpdateField[+A]:
  case Set(a: A) extends UpdateField[A]
  case Ignore extends UpdateField[Nothing]

case object NoValue

final case class ModifyCol[+A](
  add: Vector[A] = Vector(),
  delete: Vector[A] = Vector()
)

trait UpdateCField[C]:
  type E
  given col: Collection.Aux[C, E]

  def upd: ModifyCol[E]
end UpdateCField

object UpdateCField:
  def apply[C] = (col: Collection[C]) ?=> new UpdateCFieldApply[C, col.E]

  final class UpdateCFieldApply[C, A](private val dummy: Boolean = true) extends AnyVal:
    def apply(add: Vector[A] = Vector(), delete: Vector[A] = Vector())(using C: Collection.Aux[C, A]): UpdateCField[C] =
      new UpdateCField[C] {
        type E = A
        given col: Collection.Aux[C, E] = summon
        val upd = ModifyCol[E](add, delete)
      }
end UpdateCField

trait UncheckedField[Valid]:
  type Raw

  given valid: Validatable.Aux[Valid, Raw]
  def value: Raw
end UncheckedField

object UncheckedField:
  def apply[V] = (col: Validatable[V]) ?=> new UncheckedFieldApply[V, col.Raw]

  final class UncheckedFieldApply[V, R](private val dummy: Boolean = true) extends AnyVal:
    def apply(v: R)(using C: core.Validatable.Aux[V, R]): UncheckedField[V] =
      new UncheckedField[V] {
        type Raw = R
        given valid: Validatable.Aux[V, Raw] = summon
        val value = v
      }
end UncheckedField