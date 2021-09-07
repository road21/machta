package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}

object TagMatcher:
  type TupleWrapper[F[_, _ <: Tuple]] = [X, T] =>> T match
    case Tuple => F[X, T]
    case _ => F[X, Tuple1[T]]

  type InitM[X, T <: Tuple] = T match
    case (Init *: _) => X
    case (_ *: tail) => InitM[X, tail]
    case EmptyTuple => NoValue

  type ReadM[X, T <: Tuple] = X

  type UpdM[X, T <: Tuple] = T match
    case (UpdReq *: _) => X
    case (UpdCol *: _) => UpdateCollection[X]
    case (Upd *: _) => UpdateField[X]
    case (_ *: tail) => UpdM[X, tail]
    case EmptyTuple => NoValue

  type UnchAcc[X, Base <: Tuple, T <: Tuple, F[_, _ <: Tuple]] = T match
    case (Unchecked *: _) => F[Raw[X], Base]
    case (_ *: tail) => UnchAcc[X, Base, tail, F]
    case EmptyTuple => F[X, Base]

  type Unch[F[_, _ <: Tuple]] = [X, T <: Tuple] =>> UnchAcc[X, T, T, F]
  type InitUM[X, T <: Tuple] = Unch[InitM][X, T]
  type UpdUM[X, T <: Tuple] = Unch[UpdM][X, T]