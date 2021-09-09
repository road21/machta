package hkd.crud

import hkd.crud.Tags.Unchecked
import hkd.crud.Raw
import hkd.crud.TagMatcher.{UnchC, Unch, WrapTup, containsUnch, tupleWrap}
import cats.{Functor, Applicative, Traverse}

object Validate:
  def validate[H[f[_, _]], U[_, _], F[_]](raw: H[UnchC[F, U]])(using data: Data[H]): F[H[U]] = ???

  def inner[U[_, _], F[_], T, A](u: Unch[F, A, T, U])(using T: IsTag[T], traverse: Matcher[U], F: Applicative[F]): F[U[A, T]] =
    containsUnch[WrapTup[T]](tupleWrap(T.value)) match {
      case true => traverse.traverse[T].traverse(u.asInstanceOf[U[Raw[F, A], T]])(_.validate)
      case false => traverse.traverse[T].traverse(u.asInstanceOf[U[A, T]])(F.pure)
    }