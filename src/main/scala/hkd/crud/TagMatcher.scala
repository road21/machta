package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import hkd.core.{@@, DropTags, HasTag}

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

  type InitUM[X] = X match
    case a @@ (_ <: (Init & Unchecked)) => UncheckedField[DropTags[a]]
    case a @@ (_ <: Init) =>
      HasTag[a, Unchecked] match
        case true => UncheckedField[DropTags[a]]
        case false => DropTags[a]
    case a @@ (_ <: Unchecked) =>
      HasTag[a, Init] match
        case true => UncheckedField[DropTags[a]]
        case false => NoValue.type
    case a @@ _ => InitUM[a]
    case _ => NoValue.type