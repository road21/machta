package machta

import cats.data.{ValidatedNel, Validated, NonEmptyList}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.either.*
import cats.{Functor, Applicative, Monad, Traverse, Semigroup}
import org.junit.Test
import CustomTypes.{Phone, Role}
import java.time.Instant
import NoValue.noValue
import UpdateField.{Set, Ignore}
import machta.syntax.no

trait ValidationService[F[_]] {
  def phone(raw: String): F[Either[String, Phone]]
  def role(raw: String): F[Either[String, Role]]
}

object CustomTypes:
  opaque type Phone = String
  object Phone:
    def unsafeApply(str: String): Phone = str
    implicit def valid[F[_]: Functor](using v: ValidationService[F]): Validatable.Aux[ValidatedST[F], Phone, String] =
      new Validatable[ValidatedST[F], Phone] {
        type Raw = String
        def validate(r: Raw): F[ValidatedNel[String, Phone]] = v.phone(r).map(_.toValidatedNel)
      }

  opaque type Role = String
  object Role:
    def unsafeApply(str: String): Role = str
    implicit def valid[F[_]: Functor](using v: ValidationService[F]): Validatable.Aux[ValidatedST[F], Role, String] =
      new Validatable[ValidatedST[F], Role] {
        type Raw = String

        def validate(r: Raw): F[ValidatedNel[String, Role]] = v.role(r).map(_.toValidatedNel)
      }
end CustomTypes

case class User[tags[_, _]](
  id: Long no tags,
  name: Option[String] tags (Upd, Init),
  updated: Instant tags UpdReq,
  roles: Vector[Role] tags (Init, UpdCol, Unchecked),
  phone: Phone tags (Init, Upd, Unchecked)
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

class App[F[_]: Applicative]:
  given validationSvc: ValidationService[F] with
    def phone(raw: String): F[Either[String, Phone]] = {
      if(raw.forall(_.isDigit)) Right(Phone.unsafeApply(raw))
      else Left("Phone must contain only digits")
    }.pure[F]

    def role(raw: String): F[Either[String, Role]] = {
      if(raw.forall(_.isLetter)) Right(Role.unsafeApply(raw))
      else Left(s"Role must contain only digits, illegal role $raw")
    }.pure[F]

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

  val initUData = new User.RawCreate[ValidatedST[F]](
    noValue,
    Some("zopa"),
    noValue,
    Raw[Vector[Role]][ValidatedST[F]](Vector("1")),
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

  val updUData = new User.RawUpdate[ValidatedST[F]](
    noValue,
    Set(Some("zopa")),
    Instant.now,
    UpdateCol(
      add = Raw[Vector[Role]][ValidatedST[F]](Vector("2", "3")),
      delete = Raw[Vector[Role]][ValidatedST[F]](Vector("1"))
    ),
    Set(Raw[Phone]("123"))
  )

  println(initUData.validate)
  println(updUData.validate)

class Dummy:
  import cats.catsInstancesForId
  @Test def t1(): Unit = new App[[x] =>> x]
