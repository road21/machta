package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import cats.Functor

object TagMatcher:
  type TupleWrapper[U[_, _ <: Tuple]] = [X, T] =>> T match
    case Tuple => U[X, T]
    case _ => U[X, Tuple1[T]]

  type ContainsInit[T <: Tuple] <: Boolean = T match
    case EmptyTuple => false
    case h *: t => MatchInit[h, t]

  type MatchInit[H, T <: Tuple] <: Boolean = H match {
    case Init => true
    case Any => ContainsInit[T]
  }

  def containsInit[T <: Tuple](t: T): ContainsInit[T] =
    t match {
      case _: EmptyTuple => false
      case x: (h *: t) => matchInitTag[h, t](x.head, x.tail)
    }

  def matchInitTag[H, T <: Tuple](h: H, t: T): MatchInit[H, T] =
    h match {
      case _: Init => true
      case _: Any => containsInit[T](t)
    }

  type ContainsUnch[T <: Tuple] <: Boolean = T match
    case EmptyTuple => false
    case h *: t => MatchUnch[h, t]

  type MatchUnch[H, T <: Tuple] <: Boolean = H match {
    case Unchecked => true
    case Any => ContainsUnch[T]
  }

  def containsUnch[T <: Tuple](t: T): ContainsUnch[T] =
    t match {
      case _: EmptyTuple => false
      case x: (h *: t) => matchUnchTag[h, t](x.head, x.tail)
    }

  def matchUnchTag[H, T <: Tuple](h: H, t: T): MatchUnch[H, T] =
    h match {
      case _: Unchecked => true
      case _: Any => containsUnch[T](t)
    }

  type InitM[X, T <: Tuple] =
    ContainsInit[T] match
      case true => X
      case false => NoValue

  type ReadM[X, T <: Tuple] = X

  type UpdM[X, T <: Tuple] = T match
    case (UpdReq *: _) => X
    case (UpdCol *: _) => UpdateCollection[X]
    case (Upd *: _) => UpdateField[X]
    case (_ *: tail) => UpdM[X, tail]
    case EmptyTuple => NoValue

  type Unch[F[_], X, T <: Tuple, U[_, _ <: Tuple]] =
    ContainsUnch[T] match
      case true => U[Raw[F, X], T]
      case false => U[X, T]

  type UnchC[F[_], U[_, _ <: Tuple]] = [X, T <: Tuple] =>> Unch[F, X, T, U]

  type InitUM[F[_]] = [X, T <: Tuple] =>> Unch[F, X, T, InitM]
  type UpdUM[F[_]] = [X, T <: Tuple] =>> Unch[F, X, T, UpdM]

  given functorInit[Ts <: Tuple]: Functor[[x] =>> InitM[x, Ts]] with
    def map[A, B](fa: InitM[A, Ts])(f: A => B): InitM[B, Ts] =
      fa match {
        case NoValue => fa.asInstanceOf[InitM[B, Ts]]
        case x => f(x.asInstanceOf[A]).asInstanceOf[InitM[B, Ts]]
      }