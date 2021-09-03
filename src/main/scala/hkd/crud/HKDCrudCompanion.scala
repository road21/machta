package hkd.crud

trait HKDCrudCompanion[H[f[_, _ <: Tuple]]]:
  type Create = H[TagMatcher.InitM]
  type RawCreate = H[TagMatcher.InitUM]
  type Read = H[TagMatcher.ReadM]
  type Update = H[TagMatcher.UpdM]
  type RawUpdate = H[TagMatcher.UpdUM]
