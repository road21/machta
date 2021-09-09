package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import cats.{Functor, Traverse, Applicative}
import cats.syntax.applicative._

trait Matcher[U[_, _]]:
  def traverse[T]: Traverse[[x] =>> U[x, T]]

trait BiNat[A[_, _], B[_, _]]:
  def apply[X, T]: A[X, T] => B[X, T]

trait Data[H[U[_, _]]]:
  def innerTraverse[T, A[_, _]: Matcher, B[_, _], F[_]: Applicative](ha: H[A])(x: BiNat[A, [x, t] =>> F[B[x, t]]])(using T: IsTag[T]): H[B]

object TagMatcher:
  type WrapTup[T] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case h *: t => h *: t
    case Any => Tuple1[T]

  def tupleWrap[T](t : T): WrapTup[T] =
    t match {
      case _: EmptyTuple => EmptyTuple
      case x: (h *: t) => x
      case _: Any => Tuple1(t)
    }

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

  type InitM[X, T] =
    ContainsInit[WrapTup[T]] match
      case true => X
      case false => NoValue

  type ReadM[X, T] = X

  type UpdM[X, T] = WrapTup[T] match
    case (UpdReq *: _) => X
    case (UpdCol *: _) => UpdateCollection[X]
    case (Upd *: _) => UpdateField[X]
    case (_ *: tail) => UpdM[X, tail]
    case EmptyTuple => NoValue

  type Unch[F[_], X, T, U[_, _]] =
    ContainsUnch[WrapTup[T]] match
      case true => U[Raw[F, X], T]
      case false => U[X, T]

  type UnchC[F[_], U[_, _]] = [X, T] =>> Unch[F, X, T, U]

  type InitUM[F[_]] = [X, T] =>> UnchC[F, InitM][X, T]
  type UpdUM[F[_]] = [X, T] =>> UnchC[F, UpdM][X, T]

  given matcherInit: Matcher[InitM] with
    def traverse[Ts]: Traverse[[x] =>> InitM[x, Ts]] = new Traverse[[x] =>> InitM[x, Ts]] {
      def traverse[G[_]: Applicative, A, B](fa: InitM[A, Ts])(f: A => G[B]): G[InitM[B, Ts]] =
        fa match {
          case NoValue => fa.asInstanceOf[InitM[B, Ts]].pure[G]
          case x => f(x.asInstanceOf[A]).asInstanceOf[G[InitM[B, Ts]]]
        }

      def foldLeft[A, B](fa: InitM[A, Ts], b: B)(f: (B, A) => B): B = ???
      def foldRight[A, B](fa: InitM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
    }

