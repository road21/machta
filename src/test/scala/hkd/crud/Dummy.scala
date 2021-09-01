package hkd.crud

import hkd.core.{@@, Collection}
import hkd.crud.Tags.{Init, Upd, UpdCol, UpdReq, Unchecked}
import hkd.crud.UpdateField.{Ignore, Set}
import org.junit.Test

import java.time.Instant

case class MyData[F[_]](
  id: F[Long],
  name: F[Option[String] @@ (Upd & Init)],
  age: F[Int @@ (Upd & Init)],
  updated: F[Instant @@ UpdReq],
  tags: F[Vector[String] @@ (Init & UpdCol)],
  phone: F[Phone @@ (Upd & Init & Unchecked)]
)

object MyData extends HKDCrudCompanion[MyData]

class Dummy:
  val phone: Phone = ???
  val initData = new MyData.Create(NoValue, Some("zopa"), 20, NoValue, Vector("1"), phone)
  val initUData = new MyData.UncheckedCreate(NoValue, Some("zopa"), 20, NoValue, Vector("1"), "+7916")
  val updData = new MyData.Update(
    NoValue,
    Set(Some("zopa")),
    Ignore,
    Instant.now,
    UpdateCField[Vector[String]](
      add = Vector("2", "3"), delete = Vector("1")
    )
  )
  val readData = new MyData.Read(1, Some("zopa"), 20, Instant.now(), Vector("2", "3"))

  @Test def t1(): Unit = ()



