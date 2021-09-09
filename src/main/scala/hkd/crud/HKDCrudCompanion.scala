package hkd.crud

import TagMatcher._

trait HKDCrudCompanion[H[U[_, _]]]:
  type Create = H[InitM]
  type Read = H[ReadM]
  type Update = H[UpdM]
  type RawCreate[F[_]] = H[InitUM[F]]
  type RawUpdate[F[_]] = H[UpdUM[F]]