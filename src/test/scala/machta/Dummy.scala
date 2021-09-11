package machta

import cats.data.EitherT
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.{Applicative, Monad, Traverse}
import org.junit.Test
import CustomTypes.{Phone, Role}
import java.time.Instant
import NoValue.noValue
import UpdateField.{Set, Ignore}

trait ValidationService[F[_]] {
  def phone(raw: String): F[Either[String, Phone]]
  def role(raw: String): F[Either[String, Role]]
}

type EitherTC[F[_]] = [x] =>> EitherT[F, String, x]

object CustomTypes:
  opaque type Phone = String
  object Phone:
    def unsafeApply(str: String): Phone = str
    implicit def valid[F[_]: ValidationService]: Validatable.Aux[EitherTC[F], Phone, String] =
      new Validatable[EitherTC[F], Phone] {
        type Raw = String

        def validate(r: Raw): EitherT[F, String, Phone] =
          EitherT(summon[ValidationService[F]].phone(r))
      }

  opaque type Role = String
  object Role:
    def unsafeApply(str: String): Role = str
    implicit def valid[F[_]: ValidationService]: Validatable.Aux[EitherTC[F], Role, String] =
      new Validatable[EitherTC[F], Role] {
        type Raw = String

        def validate(r: Raw): EitherT[F, String, Role] =
          EitherT(summon[ValidationService[F]].role(r))
      }
end CustomTypes

case class User[@@[_, _]](
  id: Long @@ EmptyTuple,
  name: Option[String] @@ (Upd, Init),
  updated: Instant @@ UpdReq,
  roles: Vector[Role] @@ (Init, UpdCol, Unchecked),
  phone: Phone @@ (Init, Upd, Unchecked)
)

object User extends DataCompanion[User] {
  given data: Data[User] with
    def innerTraverse[A[_, _], B[_, _], F[_]: Applicative](ha: User[A])(f: MatcherTrans[A, [x, t] =>> F[B[x, t]]]): F[User[B]] =
      (
        f[Long, EmptyTuple](ha.id),
        f[Option[String], (Upd, Init)](ha.name),
        f[Instant, UpdReq](ha.updated),
        f[Vector[Role], (Init, UpdCol, Unchecked)](ha.roles),
        f[Phone, (Init, Upd, Unchecked)](ha.phone)
      ).mapN((id, name, updated, roles, phone) =>
        User[B](id, name, updated, roles, phone)
      )
}

class App[F[_]: Monad]:
  given validationSvc: ValidationService[F] with
    def phone(raw: String): F[Either[String, Phone]] = {
      if(raw.forall(_.isDigit)) Right(Phone.unsafeApply(raw))
      else Left("Phone must contain only digits")
    }.pure[F]

    def role(raw: String): F[Either[String, Role]] = Right(Role.unsafeApply(raw)).pure[F]

  val readData = new User.Read(
    1,
    Some("zopa"),
    Instant.now(),
    Vector(Role.unsafeApply("2"), Role.unsafeApply("3")),
    Phone.unsafeApply("+7991")
  )

  val initData = new User.Create(
    noValue,
    Some("zopa"),
    noValue,
    Vector(Role.unsafeApply("1")),
    Phone.unsafeApply("+7991")
  )

  val initUData = new User.RawCreate[EitherTC[F]](
    noValue,
    Some("zopa"),
    noValue,
    Raw[Vector[Role]][EitherTC[F]](Vector("1")),
    Raw[Phone]("7916")
  )

  val updData = new User.Update(
    noValue,
    Ignore,
    Instant.now,
    UpdateCol[Vector[Role]](
      add = Vector("2", "3").map(Role.unsafeApply), delete = Vector("1").map(Role.unsafeApply)
    ),
    Set(Phone.unsafeApply("+1"))
  )

  val updUData = new User.RawUpdate[EitherTC[F]](
    noValue,
    Set(Some("zopa")),
    Instant.now,
    UpdateCol(
      add = Raw[Vector[Role]][EitherTC[F]](Vector("2", "3")),
      delete = Raw[Vector[Role]][EitherTC[F]](Vector("1"))
    ),
    Set(Raw[Phone]("123"))
  )

  println(initUData.validate)
  println(updUData.validate)

class Dummy:
  import cats.catsInstancesForId
  @Test def t1(): Unit = new App[[x] =>> x]
