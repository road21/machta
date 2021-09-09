package hkd.crud

object Tags:
  type Init = Init.type
  object Init

  type Upd = Upd.type
  object Upd

  type UpdReq = UpdReq.type
  object UpdReq

  type UpdCol = UpdCol.type
  object UpdCol

  type Unchecked = Unchecked.type
  object Unchecked

trait IsTag[T]:
  def value: T

object IsTag:
  given singletons[A](using A: ValueOf[A]): IsTag[A] with
    def value: A = A.value

  given tuples[H, T <: Tuple](using h: IsTag[H], t: IsTag[T]): IsTag[H *: T] with
    def value: (H *: T) = (h.value *: t.value)
