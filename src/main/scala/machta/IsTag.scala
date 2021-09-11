package machta

trait IsTag[T]:
  def value: T

object IsTag:
  given singletons[A](using A: ValueOf[A]): IsTag[A] with
    def value: A = A.value

  given emptyTuple: IsTag[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given tuples[H, T <: Tuple](using h: IsTag[H], t: IsTag[T]): IsTag[H *: T] with
    def value: (H *: T) = (h.value *: t.value)
