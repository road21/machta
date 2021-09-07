package hkd.crud

import hkd.core.{Collection, Validatable}
import hkd.crud.Tags.{Init, Unchecked, Upd, UpdCol, UpdReq, EmptyTag}
import hkd.crud.UpdateField.{Ignore, Set}
import hkd.crud.NoValue.noValue
import CustomTypes.{Phone, Role}
import org.junit.Test
import cats.{Applicative, Traverse, Monad}
import cats.data.EitherT
import cats.syntax.applicative.*
import java.time.Instant
import TagMatcher.InitM
import cats.syntax.functor.*

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

case class MyData[@@[_, _]](
  id: Long @@ EmptyTag,
  name: Option[String] @@ (Upd | Init),
  updated: Instant @@ UpdReq,
  roles: Vector[Role] @@ (Init | UpdCol | Unchecked),
  phone: Phone @@ (Init | Upd | Unchecked)
)

object MyData extends HKDCrudCompanion[MyData]

class App[F[_]: Monad]:
  val x: InitM[Int, Init] = 10

  def zoo[S: ValueOf]: String = "zopa"

  zoo[Init]

  println(TagMatcher.initMFunctor[Init].map[Int, Int](x)(_ + 1))

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


class Dummy:
  import cats.catsInstancesForId
  @Test def t1(): Unit = new App[[x] =>> x]
