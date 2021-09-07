package hkd.crud

import TagMatcher.*

trait HKDCrudCompanion[H[f[_, _]]]:
  type Create = H[InitM]
  type RawCreate = H[InitUM]
  type Read = H[ReadM]
  type Update = H[UpdM]
  type RawUpdate = H[UpdUM]
