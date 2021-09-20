package machta

import cats.Applicative

trait DataCompanion[H[U[_, _]]]:
  type Create = H[InitM]
  type Read = H[ReadM]
  type Update = H[UpdM]
  type RawCreate[F[_]] = H[InitRawM[F]]
  type RawUpdate[F[_]] = H[UpdRawM[F]]