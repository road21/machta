package machta

import cats.{Applicative, Functor, Traverse}

trait Data[H[U[_, _]]]:
  def innerTraverse[A[_, _], B[_, _], F[_]: Applicative](ha: H[A])(x: MatcherTrans[A, [x, t] =>> F[B[x, t]]]): F[H[B]]

trait DataCompanion[H[U[_, _]]]:
  type Create = H[InitM]
  type Read = H[ReadM]
  type Update = H[UpdM]
  type RawCreate[F[_]] = H[InitRawM[F]]
  type RawUpdate[F[_]] = H[UpdRawM[F]]

  extension[H[f[_, _]], U[_, _], F[_]](raw: H[RawFormC[F, U]])
    def validate(using F: Applicative[F], data: Data[H], matcher: Matcher[U]): F[H[U]] =
      RawForm.validate(raw)
