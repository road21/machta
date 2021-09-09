package hkd.crud

import hkd.crud.Tags.Unchecked
import hkd.crud.Raw
import hkd.crud.TagMatcher.{UnchC, Unch, WrapTup, containsUnch, tupleWrap}
import cats.{Functor, Applicative, Traverse}

object Validate:
  def validate[H[f[_, _]], U[_, _], F[_]: Applicative](raw: H[UnchC[F, U]])(using data: Data[H], matcher: Matcher[U]): F[H[U]] =
    data.innerTraverse[UnchC[F, U], U, F](raw)(
      new MatcherTrans[UnchC[F, U], [x, t] =>> F[U[x, t]]] {
        def apply[X, T]: IsTag[T] ?=> UnchC[F, U][X, T] => F[U[X, T]] = T ?=> u =>
          containsUnch[WrapTup[T]](tupleWrap(T.value)) match {
            case true => matcher.traverse[T].traverse(u.asInstanceOf[U[Raw[F, X], T]])(_.validate)
            case false => matcher.traverse[T].traverse(u.asInstanceOf[U[X, T]])(Applicative[F].pure)
          }
      }
    )