package machta

import cats.{Applicative, Traverse, Eval}
import cats.syntax.apply.*
import cats.syntax.applicative.*
import cats.syntax.functor.*

enum UpdateField[+A]:
  case UpdateTo(a: A) extends UpdateField[A]
  case Skip extends UpdateField[Nothing]

object UpdateField:
  given traverse: Traverse[UpdateField] with {
    def traverse[G[_]: Applicative, A, B](fa: UpdateField[A])(f: A => G[B]): G[UpdateField[B]] =
      fa match {
        case UpdateTo(a) => f(a).map(UpdateTo.apply)
        case Skip => Skip.pure[G]
      }

    def foldLeft[A, B](fa: UpdateField[A], b: B)(f: (B, A) => B): B =
      fa match {
        case UpdateTo(a) => f(b, a)
        case Skip => b
      }

    def foldRight[A, B](fa: UpdateField[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
      fa match {
        case UpdateTo(a) => f(a, lb)
        case Skip => lb
      }
  }