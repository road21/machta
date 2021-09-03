package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}

object TagMatcher:
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

  type Unch[X, Base, T <: Tuple, F[_, _ <: Tuple]] = T match
    case (Unchecked *: _) => F[Raw[X], Base]
    case (_ *: tail) => Unch[X, Base, tail, F]
    case EmptyTuple => F[X, Base]

  type InitUM[X, T <: Tuple] = Unch[X, T, T, InitM]
  type UpdUM[X, T <: Tuple] = Unch[X, T, T, UpdM]