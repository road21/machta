package machta

import cats.{Applicative, Traverse}
import cats.syntax.traverse.*

trait Validatable[F[_], Valid]:
  type Raw

  def validate(r: Raw): F[Valid]

object Validatable:
  type Aux[f[_], V, R] = Validatable[f, V] { type Raw = R }

  implicit def validTraverse[F[_]: Applicative, C[_]: Traverse, E, R](implicit V: Validatable.Aux[F, E, R]): Validatable.Aux[F, C[E], C[R]] =
    new Validatable[F, C[E]] {
      type Raw = C[R]

      def validate(r: Raw): F[C[E]] = r.traverse(V.validate)
    }

  extension [F[_], R, V](v: R)(using V: Validatable.Aux[F, V, R])
    def validate: F[V] = V.validate(v)

trait ValidatableHKD[H[u[_, _]], U[_, _]]:
  def instance[F[_]: Applicative]: Validatable.Aux[F, H[U], H[RawFormC[F, U]]]

object ValidatableHKD:
  extension [H[u[_, _]], U[_, _], F[_]](x: H[RawFormC[F, U]])
    def validateH(using V: ValidatableHKD[H, U], F: Applicative[F]): F[H[U]] = V.instance[F].validate(x)