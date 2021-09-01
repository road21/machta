package hkd.core

trait Tagged[T, Tag]

type @@[T, Tag] = Tagged[T, Tag]

type DropTags[X] = X match
  case a @@ _ => DropTags[a]
  case _ => X

type HasTag[X, Tag] = X match
  case a @@ (_ <: Tag) => true
  case a @@ _ => HasTag[a, Tag]
  case _ => false