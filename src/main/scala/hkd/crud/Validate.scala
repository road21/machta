package hkd.crud

import TagMatcher.Unch

object Validate:
  def validate[H[f[_, _ <: Tuple]], U[_, _ <: Tuple], F[_]](raw: H[Unch[U]]): F[H[U]] = ???
