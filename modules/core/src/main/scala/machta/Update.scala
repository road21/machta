package machta

import cats.{Traverse, Applicative}
import UpdM.FindUpdTag
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*

/** Update tag  */
type Upd = Upd.type
object Upd:
  given singleTag: SingleTag[Upd] = SingleTag.instance

/** Update required tag */
type UpdReq = UpdReq.type
object UpdReq:
  given singleTag: SingleTag[UpdReq] = SingleTag.instance

/** Update collection tag */
type UpdCol = UpdCol.type
object UpdCol:
  given singleTag: SingleTag[UpdCol] = SingleTag.instance

/** Update matcher */
type UpdM[X, T] = FindUpdTag[WrapTuple[T]] match
  case Some[tag] => tag match {
    case UpdReq => X
    case UpdCol => UpdateCol[X]
    case Upd => UpdateField[X]
  }
  case None.type => NoValue

type UpdRawM[F[_]] = [X, T] =>> RawFormC[F, UpdM][X, T]

object UpdM:
  type FindUpdTag[T <: Tuple] <: Option[UpdReq | UpdCol | Upd] = T match
    case (UpdReq *: _) => Some[UpdReq]
    case (UpdCol *: _) => Some[UpdCol]
    case (Upd *: _) => Some[Upd]
    case (_ *: tail) => FindUpdTag[tail]
    case EmptyTuple => None.type

  given traverseUpdReq[Ts](using C: Ts Contains UpdReq): Traverse[[x] =>> UpdM[x, Ts]] with
    def cast[A](a: UpdM[A, Ts]): A = a.asInstanceOf

    def traverse[G[_]: Applicative, A, B](fa: UpdM[A, Ts])(f: A => G[B]): G[UpdM[B, Ts]] = cast(fa).asInstanceOf[G[UpdM[B, Ts]]]
    def foldLeft[A, B](fa: UpdM[A, Ts], b: B)(f: (B, A) => B): B = f(b, cast(fa))
    def foldRight[A, B](fa: UpdM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = f(cast(fa), lb)

  given traverseUpdCol[Ts](using C: Ts NotContains UpdReq, C2: Ts Contains UpdCol): Traverse[[x] =>> UpdM[x, Ts]] with
    def cast[A](a: UpdM[A, Ts]): UpdateCol[A] = a.asInstanceOf

    def traverse[G[_]: Applicative, A, B](fa: UpdM[A, Ts])(f: A => G[B]): G[UpdM[B, Ts]] = cast(fa).traverse(f).asInstanceOf
    def foldLeft[A, B](fa: UpdM[A, Ts], b: B)(f: (B, A) => B): B = cast(fa).foldLeft(b)(f)
    def foldRight[A, B](fa: UpdM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = cast(fa).foldRight(lb)(f)

  given traverseUpd[Ts](using C: Ts NotContains (UpdReq, UpdCol), C3: Ts Contains Upd): Traverse[[x] =>> UpdM[x, Ts]] with
    def cast[A](a: UpdM[A, Ts]): UpdateField[A] = a.asInstanceOf

    def traverse[G[_]: Applicative, A, B](fa: UpdM[A, Ts])(f: A => G[B]): G[UpdM[B, Ts]] = cast(fa).traverse(f).asInstanceOf
    def foldLeft[A, B](fa: UpdM[A, Ts], b: B)(f: (B, A) => B): B = cast(fa).foldLeft(b)(f)
    def foldRight[A, B](fa: UpdM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = cast(fa).foldRight(lb)(f)

  given traverseIgnore[Ts](using C: Ts NotContains (UpdReq, UpdCol, Upd)): Traverse[[x] =>> UpdM[x, Ts]] with
    def traverse[G[_]: Applicative, A, B](fa: UpdM[A, Ts])(f: A => G[B]): G[UpdM[B, Ts]] = NoValue.asInstanceOf
    def foldLeft[A, B](fa: UpdM[A, Ts], b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: UpdM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = lb
