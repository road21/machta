package machta

import cats.Applicative
import cats.syntax.apply.*
import machta.{Phone, Role}
import machta.syntax.no
import machta.RawFormV.validateR

case class User[tags[_, _]](
  id: Long no tags,
  name: Option[String] tags (Upd, Init),
  roles: Vector[Role] tags (Init, UpdCol, Unchecked),
  phone: Phone tags (Init, Upd, Unchecked)
)

trait ValidatableHKD[H[u[_, _]], U[_, _]]:
  def instance[F[_]: Applicative]: Validatable.Aux[F, H[U], H[RawFormC[F, U]]]

object User extends DataCompanion[User] {
  // TODO: derive this
  given updMValidate: ValidatableHKD[User, UpdM] with
    def instance[F[_]: Applicative]: Validatable.Aux[F, User[UpdM], User[RawFormC[F, UpdM]]] = new Validatable[F, User[UpdM]] {
      type Raw = User[UpdRawM[F]]

      def validate(r: User[UpdRawM[F]]): F[User[UpdM]] =
        (r.id.validateR, r.name.validateR, r.roles.validateR, r.phone.validateR).mapN((id, name, roles, phone) =>
          User[UpdM](id, name, roles, phone)
        )
    }

  given initMValidate: ValidatableHKD[User, InitM] with
    def instance[F[_]: Applicative]: Validatable.Aux[F, User[InitM], User[RawFormC[F, InitM]]] = new Validatable[F, User[InitM]] {
      type Raw = User[RawFormC[F, InitM]]

      def validate(r: User[RawFormC[F, InitM]]): F[User[InitM]] =
        (r.id.validateR, r.name.validateR, r.roles.validateR, r.phone.validateR).mapN((id, name, roles, phone) =>
          User[InitM](id, name, roles, phone)
        )
    }

  extension [H[u[_, _]], U[_, _], F[_]](x: H[RawFormC[F, U]])
    def validateH(using V: ValidatableHKD[H, U], F: Applicative[F]): F[H[U]] = V.instance[F].validate(x)
}