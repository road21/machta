package hkd.crud

import hkd.core.@@
import hkd.crud.Tags.{ReadUpd, ReadUpdInit, Read, Upd, Init}
import hkd.crud.UpdateField.{Set, Ignore}
import org.junit.Test
import java.time.Instant

case class MyData[F[_]](
  id: F[Long @@ Read],
  name: F[Option[String] @@ ReadUpdInit],
  age: F[Int @@ ReadUpdInit],
  updated: F[Instant @@ ReadUpd]
)

object MyData extends HKDCrudCompanion[MyData]

class Dummy {
  val initData = new MyData.Create(NoValue, Some("zopa"), 20, NoValue)
  val updData = new MyData.Update(NoValue, Set(Some("zopa")), Ignore, Set(Instant.now))
  val readData = new MyData.Read(1, Some("zopa"), 20, Instant.now())

  @Test def t1(): Unit = ()
}



