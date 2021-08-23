package hkd.crud

object Tags {
  trait Init
  trait Read
  trait Upd

  type ReadUpd = Read & Upd
  type ReadInit = Read & Init
  type ReadUpdInit = Read & Init & Upd
}