package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import cats.{Applicative, Functor, Traverse}
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import UpdateTag.UpdM
import TagMatcher.{WrapTup, UnchC}

trait Matcher[U[_, _]]:
  def traverse[T]: Traverse[[x] =>> U[x, T]]

trait MatcherTrans[A[_, _], B[_, _]]:
  def apply[X, T]: IsTag[T] ?=> A[X, T] => B[X, T]

trait Data[H[U[_, _]]]:
  def innerTraverse[A[_, _], B[_, _], F[_]: Applicative](ha: H[A])(x: MatcherTrans[A, [x, t] =>> F[B[x, t]]]): F[H[B]]

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

  type InitM[X, T] = ContainsInit[WrapTup[T]] match
    case true => X
    case false => NoValue

  type ReadM[X, T] = X

  type Unch[F[_], X, T, U[_, _]] =
    ContainsUnch[WrapTup[T]] match
      case true => U[Raw[F, X], T]
      case false => U[X, T]

  type UnchC[F[_], U[_, _]] = [X, T] =>> Unch[F, X, T, U]

  type InitUM[F[_]] = [X, T] =>> UnchC[F, InitM][X, T]

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

object UpdateTag:
  type FindUpdTag[T <: Tuple] <: Option[UpdReq | UpdCol | Upd] = T match
    case (UpdReq *: _) => Some[UpdReq]
    case (UpdCol *: _) => Some[UpdCol]
    case (Upd *: _) => Some[Upd]
    case (_ *: tail) => FindUpdTag[tail]
    case EmptyTuple => None.type

  type UpdM[X, T] = FindUpdTag[WrapTup[T]] match
    case Some[tag] => tag match {
      case UpdReq => X
      case UpdCol => ModifyCol[X]
      case Upd => UpdateField[X]
    }
    case None.type => NoValue

  type UpdUM[F[_]] = [X, T] =>> UnchC[F, UpdM][X, T]

  given matcherUpd: Matcher[UpdM] with
    def traverse[Ts]: Traverse[[x] =>> UpdM[x, Ts]] = new Traverse[[x] =>> UpdM[x, Ts]] {
      def traverse[G[_]: Applicative, A, B](fa: UpdM[A, Ts])(f: A => G[B]): G[UpdM[B, Ts]] =
        fa match {
          case NoValue => fa.asInstanceOf[UpdM[B, Ts]].pure[G]
          case m: ModifyCol[_] => Traverse[ModifyCol].traverse(m.asInstanceOf[ModifyCol[A]])(f).asInstanceOf[G[UpdM[B, Ts]]]
          case u: UpdateField[_] => Traverse[UpdateField].traverse(u.asInstanceOf[UpdateField[A]])(f).asInstanceOf[G[UpdM[B, Ts]]]
          case x => f(x.asInstanceOf[A]).asInstanceOf[G[UpdM[B, Ts]]]
        }

      def foldLeft[A, B](fa: UpdM[A, Ts], b: B)(f: (B, A) => B): B = ???
      def foldRight[A, B](fa: UpdM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
    }