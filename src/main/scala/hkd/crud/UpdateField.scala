package hkd.crud

enum UpdateField[+A]:
  case Set(a: A) extends UpdateField[A]
  case Ignore extends UpdateField[Nothing]

case object NoValue