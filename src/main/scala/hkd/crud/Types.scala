package hkd.crud

import hkd.core.@@
import hkd.crud.Tags.{Init, Read, Upd}

object Types {
  type DropTags[X] = X match
    case a @@ _ => DropTags[a]
    case _ => X

  type I[X] = X match
    case a @@ (_ <: Init) => DropTags[a]
    case a @@ _ => I[a]
    case _ => NoValue.type

  type R[X] = X match
    case a @@ (_ <: Read) => DropTags[a]
    case a @@ _ => R[a]
    case _ => NoValue.type

  type U[X] = X match
    case a @@ (_ <: Upd) => UpdateField[DropTags[a]]
    case a @@ _ => U[a]
    case _ => NoValue.type
}