package hkd.crud

import Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import cats.Functor

final case class Inv[T](t: T)

trait IsTag[x]:
  def tags: x

object IsTag:
  given tag[T](using v: ValueOf[T]): IsTag[T] with
    def tags: T = v.value

object TagMatcher:
  type InitM[X, T] = Inv[T] match
    case Inv[_ >: Init] => X
    case Inv[_] => NoValue

  type ReadM[X, T] = X

  type UpdM[X, T] = Inv[T] match
    case Inv[_ >: UpdReq] => X
    case Inv[_ >: UpdCol] => UpdateCollection[X]
    case Inv[_ >: Upd] => UpdateField[X]
    case Inv[_] => NoValue

  type Unch[F[_, _]] = [X, T] =>> Inv[T] match
    case Inv[_ >: Unchecked] => F[Raw[X], T]
    case Inv[_] => F[X, T]

  type InitUM[X, T] = Unch[InitM][X, T]
  type UpdUM[X, T] = Unch[UpdM][X, T]

  def initMFunctor[T](using t: IsTag[T]): Functor[[x] =>> InitM[x, T]] = new Functor[[x] =>> InitM[x, T]] {
    def map[A, B](fa: InitM[A, T])(f: A => B): InitM[B, T] =
      Inv[T](t.tags) match
        case Inv(Init) => f(fa.asInstanceOf[A]).asInstanceOf[InitM[B, T]]
        case _ => NoValue.noValue.asInstanceOf[InitM[B, T]]
  }