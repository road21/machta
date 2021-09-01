package hkd.core

trait Validatable[Valid]:
  type Raw

object Validatable:
  type Aux[V, R] = Validatable[V] { type Raw = R }