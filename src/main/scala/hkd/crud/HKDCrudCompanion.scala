package hkd.crud

import TagMatcher._

trait HKDCrudCompanion[H[U[_, _]]]:
  type Create = H[TupleWrapper[InitM]]
  type Read = H[TupleWrapper[ReadM]]
  type Update = H[TupleWrapper[UpdM]]
  type RawCreate[F[_]] = H[TupleWrapper[InitUM[F]]]
  type RawUpdate[F[_]] = H[TupleWrapper[UpdUM[F]]]