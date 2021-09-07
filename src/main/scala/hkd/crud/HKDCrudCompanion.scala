package hkd.crud

import TagMatcher._

trait HKDCrudCompanion[H[f[_, _]]]:
  type Create = H[TupleWrapper[InitM]]
  type RawCreate = H[TupleWrapper[InitUM]]
  type Read = H[TupleWrapper[ReadM]]
  type Update = H[TupleWrapper[UpdM]]
  type RawUpdate = H[TupleWrapper[UpdUM]]
