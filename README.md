## Hkd Crud  

### Usage
```scala
  import hkd.core.@@
  import hkd.crud.Tags.{Read, Upd, Init}
  import hkd.crud.UpdateField.{Set, Ignore}
  import hkd.crud.{NoValue, HKDCrudCompanion}
  import java.time.Instant

  case class MyData[F[_]](
    id: F[Long @@ Read],
    name: F[Option[String] @@ (Read & Upd & Init)],
    age: F[Int @@ (Read & Upd & Init)],
    updated: F[Instant @@ (Read & Upd)]
  )

  object MyData extends HKDCrudCompanion[MyData]
  
  val initData = new MyData.Create(NoValue, Some("zopa"), 20, NoValue)
  val updData = new MyData.Update(NoValue, Set(Some("zopa")), Ignore, Set(Instant.now))
  val readData = new MyData.Read(1, Some("zopa"), 20, Instant.now())
```
