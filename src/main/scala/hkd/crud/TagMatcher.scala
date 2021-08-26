package hkd.crud

import hkd.crud.Tags.UpdReq
import hkd.core.{@@, DropTags}
import Tags.{Init, Read, Upd, UpdReq}

object TagMatcher {
  type Find[X, Tag, F[_]] = X match
    case a @@ (_ <: Tag) => F[DropTags[a]]
    case a @@ _ => InitM[a]
    case _ => NoValue.type

  type InitM[X] = Find[X, Init, [x] =>> x]
  type ReadM[X] = Find[X, Read, [x] =>> x]
  type UpdM[X] = Find[X, Upd, UpdateField]
  type UpdReqM[X] = Find[X, UpdReq, [x] =>> x]
}
