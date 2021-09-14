package machta

type WrapTuple[T] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case h *: t => h *: t
  case Any => Tuple1[T]

object WrapTuple:
  def wrapTuple[T](t : T): WrapTuple[T] =
    t match {
      case _: EmptyTuple => EmptyTuple
      case x: (h *: t) => x
      case _: Any => Tuple1(t)
    }