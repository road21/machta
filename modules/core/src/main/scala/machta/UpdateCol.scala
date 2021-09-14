package machta

import cats.{Applicative, Traverse, Eval}
import cats.syntax.apply.*

final case class UpdateCol[+A](add: A, delete: A)

object UpdateCol:
  given modifyTraverse: Traverse[UpdateCol] with {
    def traverse[G[_]: Applicative, A, B](fa: UpdateCol[A])(f: A => G[B]): G[UpdateCol[B]] = 
      (f(fa.add), f(fa.delete)).mapN(UpdateCol.apply)
    def foldLeft[A, B](fa: UpdateCol[A], b: B)(f: (B, A) => B): B = 
      f(f(b, fa.add), fa.delete)
    def foldRight[A, B](fa: UpdateCol[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = 
      f(fa.add, f(fa.delete, lb))
  }
