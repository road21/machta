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

  given traverseInit[Ts](using C: Contains[Ts, Init]): Traverse[[x] =>> InitM[x, Ts]] with
    def dcast[A](a: InitM[A, Ts]): A = a.asInstanceOf

    def traverse[G[_]: Applicative, A, B](fa: InitM[A, Ts])(f: A => G[B]): G[InitM[B, Ts]] = f(dcast(fa)).asInstanceOf
    def foldLeft[A, B](fa: InitM[A, Ts], b: B)(f: (B, A) => B): B = f(b, dcast(fa))
    def foldRight[A, B](fa: InitM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = f(dcast(fa), lb)

  given traverseNoValue[Ts](using C: NotContains[Ts, Init]): Traverse[[x] =>> InitM[x, Ts]] with
    def traverse[G[_]: Applicative, A, B](fa: InitM[A, Ts])(f: A => G[B]): G[InitM[B, Ts]] =
      NoValue.asInstanceOf[InitM[B, Ts]].pure[G]
    def foldLeft[A, B](fa: InitM[A, Ts], b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: InitM[A, Ts], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = lb