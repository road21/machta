package hkd.crud

import hkd.core
import hkd.core.Validatable
import cats.{Traverse, Applicative}
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.applicative.*

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


final case class ModifyCol[+A](add: A, delete: A)

object ModifyCol:
  given modifyTraverse: Traverse[ModifyCol] with {
    def traverse[G[_]: Applicative, A, B](fa: ModifyCol[A])(f: A => G[B]): G[ModifyCol[B]] = (f(fa.add), f(fa.delete)).mapN(ModifyCol.apply)
    def foldLeft[A, B](fa: ModifyCol[A], b: B)(f: (B, A) => B): B = f(f(b, fa.add), fa.delete)
    def foldRight[A, B](fa: ModifyCol[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = f(fa.add, f(fa.delete, lb))
  }

trait Raw[F[_], Valid]:
  type R

  def valid: Validatable.Aux[F, Valid, R]
  def value: R

  def validate: F[Valid] = valid.validate(value)
end Raw

object Raw:
  def apply[V] = [F[_]] => (vld: Validatable[F, V]) ?=> (v: vld.Raw) =>
    new Raw[F, V] {
      type R = vld.Raw
      val valid: Validatable.Aux[F, V, R] = summon
      val value = v
    }
end Raw