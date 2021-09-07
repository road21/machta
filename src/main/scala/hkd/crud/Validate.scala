package hkd.crud

import TagMatcher.Unch

object Validate:
  def validate[H[f[_, _ <: Tuple]], U[_, _ <: Tuple], F[_]](raw: H[Unch[U]]): F[H[U]] = ???

//  def inner[H[f[_, _ <: Tuple]], U[_, _ <: Tuple], F[_], A, B, C]: Unch[U][A, B] => F[U[A, C]]] =