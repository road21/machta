package machta

object syntax:
  type no[A, @@[_, _]] = A @@ EmptyTuple
  type -- = EmptyTuple