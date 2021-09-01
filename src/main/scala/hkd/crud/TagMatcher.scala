package hkd.crud

import hkd.crud.Tags.UpdReq
import hkd.core.{@@, DropTags}
import Tags.{Init, Upd, UpdReq, UpdCol}

object TagMatcher:
  type Find[X, Tag, F[_]] = X match
    case a @@ (_ <: Tag) => F[DropTags[a]]
    case a @@ _ => Find[a, Tag, F]
    case _ => NoValue.type

  type InitM[X] = Find[X, Init, [x] =>> x]
  type ReadM[X] = DropTags[X]

  type UpdM[X] = X match
    case a @@ (_ <: UpdReq) => DropTags[a]
    case a @@ (_ <: UpdCol) => UpdateCField[DropTags[a]]
    case a @@ (_ <: Upd) => UpdateField[DropTags[a]]
    case a @@ _ => UpdM[a]
    case _ => NoValue.type