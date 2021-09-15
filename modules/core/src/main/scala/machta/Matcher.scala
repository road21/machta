package machta

import cats.Traverse

trait Matcher[U[_, _]]:
  def traverse[T]: Traverse[[x] =>> U[x, T]]

trait MatcherTrans[A[_, _], B[_, _]]:
  def apply[X, T]: Tags[T] ?=> A[X, T] => B[X, T]
