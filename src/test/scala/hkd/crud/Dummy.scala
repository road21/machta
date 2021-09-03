package hkd.crud

import hkd.core.{Collection, Validatable}
import hkd.crud.Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import hkd.crud.UpdateField.{Ignore, Set}
import hkd.crud.NoValue.noValue
import CustomTypes.{Phone, Role}
import org.junit.Test

import java.time.Instant

object CustomTypes:
  opaque type Phone = String
  object Phone:
    def unsafeApply(str: String): Phone = str
    implicit val valid: Validatable.Aux[Phone, String] = new Validatable[Phone] { type Raw = String }

  opaque type Role = String
  object Role:
    def unsafeApply(str: String): Role = str
    implicit val valid: Validatable.Aux[Role, String] = new Validatable[Role] { type Raw = String }
end CustomTypes

case class MyData[@@[_, _ <: Tuple]](
  id: Long @@ EmptyTuple,
  name: Option[String] @@ (Upd *: Init *: EmptyTuple),
  updated: Instant @@ (UpdReq *: EmptyTuple),
  roles: Vector[Role] @@ (Init *: UpdCol *: Unchecked *: EmptyTuple),
  phone: Phone @@ (Init *: Upd *: Unchecked *: EmptyTuple)
)

object MyData extends HKDCrudCompanion[MyData]

class Dummy:
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
    Raw[Vector[Role]](Vector("1")),
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

  @Test def t1(): Unit = ()
