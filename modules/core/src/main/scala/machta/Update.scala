package machta

import cats.{Traverse, Applicative}
import UpdM.FindUpdTag
import cats.syntax.applicative.*

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

  given matcherInstance: Matcher[UpdM] with
    def traverse[Ts]: Traverse[[x] =>> UpdM[x, Ts]] = new Traverse[[x] =>> UpdM[x, Ts]] {
      def traverse[G[_]: Applicative, A, B](fa: UpdM[A, Ts])(f: A => G[B]): G[UpdM[B, Ts]] =
        fa match {
          case NoValue => fa.asInstanceOf[UpdM[B, Ts]].pure[G]
          case m: UpdateCol[_] =>
            Traverse[UpdateCol].traverse(m.asInstanceOf[UpdateCol[A]])(f).asInstanceOf[G[UpdM[B, Ts]]]
          case u: UpdateField[_] =>
            Traverse[UpdateField].traverse(u.asInstanceOf[UpdateField[A]])(f).asInstanceOf[G[UpdM[B, Ts]]]
          case x => f(x.asInstanceOf[A]).asInstanceOf[G[UpdM[B, Ts]]]
        }

      def foldLeft[A, B](fa: UpdM[A, Ts], b: B)(f: (B, A) => B): B = ???
      def foldRight[A, B](fa: UpdM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
    }