package hkd.crud

import hkd.core.{Collection, Validatable}
import hkd.crud.Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import hkd.crud.UpdateField.{Ignore, Set}
import hkd.crud.NoValue.noValue
import CustomTypes.{Phone, Role}
import org.junit.Test
import cats.{Applicative, Traverse, Monad}
import cats.data.EitherT
import cats.syntax.applicative._
import java.time.Instant

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

case class MyData[@@[_, _ <: Tuple]](
  id: Long @@ EmptyTuple,
  name: Option[String] @@ (Upd, Init),
  updated: Instant @@ (UpdReq, EmptyTuple),
  roles: Vector[Role] @@ (Init, UpdCol, Unchecked),
  phone: Phone @@ (Init, Upd, Unchecked, EmptyTuple)
)

object MyData extends HKDCrudCompanion[MyData]

class App[F[_]: Monad]:
  given validationSvc: ValidationService[F] with
    def phone(raw: String): F[Either[String, Phone]] = Right(Phone.unsafeApply(raw)).pure[F]
    def role(raw: String): F[Either[String, Role]] = Right(Role.unsafeApply(raw)).pure[F]

  val readData = new MyData.Read(
    1,
    Some("zopa"),
    Instant.now(),
    Vector(Role.unsafeApply("2"), Role.unsafeApply("3")),
    Phone.unsafeApply("+7991")
  )

  val initData = new MyData.Create(
    noValue,
    Some("zopa"),
    noValue,
    Vector(Role.unsafeApply("1")),
    Phone.unsafeApply("+7991")
  )

  val initUData = new MyData.RawCreate(
    noValue,
    Some("zopa"),
    noValue,
    Raw[Vector[Role]][EitherTC[F]](Vector("1")),
    Raw[Phone]("+7916")
  )

  val updData = new MyData.Update(
    noValue,
    Ignore,
    Instant.now,
    UpdateCollection[Vector[Role]](
      add = Vector("2", "3").map(Role.unsafeApply), delete = Vector("1").map(Role.unsafeApply)
    ),
    Set(Phone.unsafeApply("+1"))
  )

  val updUData = new MyData.RawUpdate(
    noValue,
    Set(Some("zopa")),
    Instant.now,
    UpdateCollection[Raw[Vector[Role]]](
      add = Vector("2", "3"), delete = Vector("1")
    ),
    Set(Raw[Phone]("zopa"))
  )


object Dummy:
  import cats.catsInstancesForId
  @Test def t1(): Unit = new App[[x] =>> x]
