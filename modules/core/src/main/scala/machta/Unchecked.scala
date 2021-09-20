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

  def validatableNotRaw[F[_]: Applicative, U[_, _], X, T](using nc: NotContains[T, Unchecked]): Validatable.Aux[F, U[X, T], RawForm[F, X, T, U]] = new Validatable[F, U[X, T]] {
    type Raw = RawForm[F, X, T, U]
    def validate(r: RawForm[F, X, T, U]): F[U[X, T]] = (r.asInstanceOf[U[X, T]]).pure[F]
  }

  def validatableRaw[F[_]: Applicative, U[_, _], X, T](using c: Contains[T, Unchecked], T: Traverse[[x] =>> U[x, T]]): Validatable.Aux[F, U[X, T], RawForm[F, X, T, U]] = new Validatable[F, U[X, T]] {
    type Raw = RawForm[F, X, T, U]
    def validate(r: RawForm[F, X, T, U]): F[U[X, T]] =
      T.traverse(r.asInstanceOf[U[machta.Raw[F, X], T]])(_.validate)
  }

/** for better instance derivation */
trait RawFormV[T]:
  def instance[U[_, _], X, F[_]: Applicative](using T: Traverse[[x] =>> U[x, T]]): Validatable.Aux[F, U[X, T], RawForm[F, X, T, U]]

object RawFormV:
  given rawFormVNotRaw[T](using nc: NotContains[T, Unchecked]): RawFormV[T] with
    def instance[U[_, _], X, F[_]: Applicative](using T: Traverse[[x] =>> U[x, T]]): Validatable.Aux[F, U[X, T], RawForm[F, X, T, U]] =
      RawForm.validatableNotRaw[F, U, X, T]

  given rawFormVRaw[T](using nc: Contains[T, Unchecked]): RawFormV[T] with
    def instance[U[_, _], X, F[_]: Applicative](using T: Traverse[[x] =>> U[x, T]]): Validatable.Aux[F, U[X, T], RawForm[F, X, T, U]] =
      RawForm.validatableRaw[F, U, X, T]

  extension [T, U[_, _], X, F[_]](x: RawForm[F, X, T, U])
    def validateR(using F: Applicative[F], T: Traverse[[x] =>> U[x, T]], V: RawFormV[T]): F[U[X, T]] = V.instance[U, X, F].validate(x)
