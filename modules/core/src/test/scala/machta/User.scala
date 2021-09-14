package machta

import cats.Applicative
import cats.syntax.apply.*
import machta.{Phone, Role}
import machta.syntax.no

case class User[tags[_, _]](
  id: Long no tags,
  name: Option[String] tags (Upd, Init),
  roles: Vector[Role] tags (Init, UpdCol, Unchecked),
  phone: Phone tags (Init, Upd, Unchecked)
)

object User extends DataCompanion[User] {
  given data: Data[User] with
    def innerTraverse[A[_, _], B[_, _], F[_]: Applicative](ha: User[A])(f: MatcherTrans[A, [x, t] =>> F[B[x, t]]]): F[User[B]] =
      (
        f[Long, EmptyTuple](ha.id),
        f[Option[String], (Upd, Init)](ha.name),
        f[Vector[Role], (Init, UpdCol, Unchecked)](ha.roles),
        f[Phone, (Init, Upd, Unchecked)](ha.phone)
        ).mapN((id, name, roles, phone) =>
        User[B](id, name, roles, phone)
      )
}