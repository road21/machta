package hkd.crud

trait HKDCrudCompanion[H[f[_]]] {
  type Create = H[TagMatcher.InitM]
  type Read = H[TagMatcher.ReadM]
  type Update = H[TagMatcher.UpdM]
}
