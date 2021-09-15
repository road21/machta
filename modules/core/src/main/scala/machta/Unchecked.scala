package machta

import cats.{Applicative, Traverse, Eval}
import cats.syntax.applicative.*

import RawForm.ContainsUnch

type Unchecked = Unchecked.type
object Unchecked:
  given singleTag: SingleTag[Unchecked] = SingleTag.instance

/** Raw form of matcher U[_, _] */
type RawForm[F[_], X, T, U[_, _]] =
  ContainsUnch[WrapTuple[T]] match
    case true => U[Raw[F, X], T]
    case false => U[X, T]

type RawFormC[F[_], U[_, _]] = [X, T] =>> RawForm[F, X, T, U]

object RawForm:
  type ContainsUnch[T <: Tuple] <: Boolean = T match
    case EmptyTuple => false
    case h *: t => MatchUnch[h, t]

  def containsUnch[T <: Tuple](t: T): ContainsUnch[T] =
    t match {
      case _: EmptyTuple => false
      case x: (h *: t) => matchUnch[h, t](x.head, x.tail)
    }

  type MatchUnch[H, T <: Tuple] <: Boolean = H match {
    case Unchecked => true
    case Any => ContainsUnch[T]
  }

  def matchUnch[H, T <: Tuple](h: H, t: T): MatchUnch[H, T] =
    h match {
      case _: Unchecked => true
      case _: Any => containsUnch[T](t)
    }

  def validate[H[f[_, _]], U[_, _], F[_]: Applicative](raw: H[RawFormC[F, U]])(using data: Data[H], matcher: Matcher[U]): F[H[U]] =
    data.innerTraverse[RawFormC[F, U], U, F](raw)(
      new MatcherTrans[RawFormC[F, U], [x, t] =>> F[U[x, t]]] {
        def apply[X, T]: Tags[T] ?=> RawFormC[F, U][X, T] => F[U[X, T]] = T ?=> u =>
          containsUnch[WrapTuple[T]](WrapTuple.wrapTuple(T.value)) match {
            case true => matcher.traverse[T].traverse(u.asInstanceOf[U[Raw[F, X], T]])(_.validate)
            case false => matcher.traverse[T].traverse(u.asInstanceOf[U[X, T]])(Applicative[F].pure)
          }
      }
    )
