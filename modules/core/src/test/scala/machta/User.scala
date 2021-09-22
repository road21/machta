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

object User extends DataCompanion[User] with ValidatableInstances

trait ValidatableInstances1 {
  // TODO: derive this
  given updMValidate: ValidatableHKD[User, UpdM] with
    def instance[F[_]: Applicative]: Validatable.Aux[F, User[UpdM], User[RawFormC[F, UpdM]]] = new Validatable[F, User[UpdM]] {
      type Raw = User[UpdRawM[F]]

      def validate(r: User[UpdRawM[F]]): F[User[UpdM]] =
        (r.id.validateR, r.name.validateR, r.roles.validateR, r.phone.validateR).mapN((id, name, roles, phone) =>
          User[UpdM](id, name, roles, phone)
        )
    }
}

trait ValidatableInstances extends ValidatableInstances1 {
  given initMValidate: ValidatableHKD[User, InitM] with
    def instance[F[_]: Applicative]: Validatable.Aux[F, User[InitM], User[RawFormC[F, InitM]]] = new Validatable[F, User[InitM]] {
      type Raw = User[RawFormC[F, InitM]]

      def validate(r: User[RawFormC[F, InitM]]): F[User[InitM]] =
        (r.id.validateR, r.name.validateR, r.roles.validateR, r.phone.validateR).mapN((id, name, roles, phone) =>
          User[InitM](id, name, roles, phone)
        )
    }
}