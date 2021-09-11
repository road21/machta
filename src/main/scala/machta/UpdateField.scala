package machta

import cats.{Applicative, Traverse, Eval}
import cats.syntax.apply.*
import cats.syntax.applicative.*
import cats.syntax.functor.*

enum UpdateField[+A]:
  case Set(a: A) extends UpdateField[A]
  case Ignore extends UpdateField[Nothing]

object UpdateField:
  given traverse: Traverse[UpdateField] with {
    def traverse[G[_]: Applicative, A, B](fa: UpdateField[A])(f: A => G[B]): G[UpdateField[B]] =
      fa match {
        case Set(a) => f(a).map(Set.apply)
        case Ignore => Ignore.pure[G]
      }

    def foldLeft[A, B](fa: UpdateField[A], b: B)(f: (B, A) => B): B =
      fa match {
        case Set(a) => f(b, a)
        case Ignore => b
      }

    def foldRight[A, B](fa: UpdateField[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
      fa match {
        case Set(a) => f(a, lb)
        case Ignore => lb
      }
  }