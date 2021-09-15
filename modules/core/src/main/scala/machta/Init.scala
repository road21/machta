package machta

import cats.{Applicative, Traverse}
import cats.syntax.applicative.*
import machta.InitM.ContainsInit

/** Init tag */
type Init = Init.type
object Init:
  given singleTag: SingleTag[Init] = SingleTag.instance

/** Init tag matcher */
type InitM[X, T] = ContainsInit[WrapTuple[T]] match
  case true => X
  case false => NoValue

type InitRawM[F[_]] = [X, T] =>> RawFormC[F, InitM][X, T]

object InitM:
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
