package hkd.crud

import hkd.crud.Tags.Unchecked
import hkd.crud.Raw
import hkd.crud.TagMatcher.{UnchC, Unch, containsUnch}
import cats.{Functor, Applicative}

object Validate:
  def validate[H[f[_, _ <: Tuple]], U[_, _ <: Tuple], F[_]](raw: H[UnchC[F, U]]): F[H[U]] = ???

  def inner[U[_, _ <: Tuple], F[_], T <: Tuple, A](u: Unch[F, A, T, U])(using T: IsTag[T], functor: Functor[[x] =>> U[x, T]], F: Applicative[F]): U[F[A], T] =
    containsUnch[T](T.value) match {
      case true => functor.map(u.asInstanceOf[U[Raw[F, A], T]])(_.validate)
      case false => functor.map(u.asInstanceOf[U[A, T]])(F.pure)
    }





