package machta

import Tuple.Concat
import scala.collection.View.Empty

final case class SingleTag[T](value: T)

object SingleTag:
  def instance[T](using v: ValueOf[T]): SingleTag[T] = SingleTag[T](v.value)

final case class Tags[T](value: T)

object Tags:
  given single[A](using s: SingleTag[A]): Tags[A] = Tags(s.value)
  given empty: Tags[EmptyTuple] = Tags(EmptyTuple)
  given tuples[H, T <: Tuple](using h: Tags[H], t: Tags[T]): Tags[H *: T] = Tags(h.value *: t.value)

final class Contains[Ts, T]

object Contains:
  def instance[Ts, T: SingleTag]: Ts Contains T = Contains[Ts, T]

  given identity[A: SingleTag]: Contains[A, A] = instance
  given head[Head: SingleTag, Tail <: Tuple]: Contains[Head *: Tail, Head] = instance
  given tail[Head, Tail <: Tuple, T: SingleTag](using c: Tail Contains T): Contains[Head *: Tail, T] = instance

final class NotContains[Ts, T]

object NotContains:
  type Eq[X, Y] <: Boolean = X match {
    case Y => true
    case _ => false
  }

  def instance[Ts, T: SingleTag]: Ts NotContains T = NotContains[Ts, T]

  given different[X: SingleTag, Y: SingleTag](using neq: Eq[X, Y] =:= false): NotContains[X, Y] = instance
  given empty[X: SingleTag]: NotContains[EmptyTuple, X] = instance
  given head[X: SingleTag, T: SingleTag, Tail <: Tuple](using ncX: X NotContains T, ncT: Tail NotContains T): NotContains[X *: Tail, T] = instance