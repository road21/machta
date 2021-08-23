package hkd.crud

trait HKDCrudCompanion[H[f[_]]] {
  type Create = H[Types.I]
  type Read = H[Types.R]
  type Update = H[Types.U]
}
