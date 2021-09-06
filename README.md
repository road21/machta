## Hkd Crud  

### Idea
Tag your hkd data class fields:
```scala
import hkd.crud.Tags.{Init, Unchecked, Upd, UpdCol, UpdReq}
import YourCustomTypes.{Phone, Role}
import java.time.Instant

case class MyData[@@[_, _ <: Tuple]](
  id: Long @@ EmptyTuple,
  name: Option[String] @@ (Upd, Init),
  updated: Instant @@ (UpdReq, EmptyTuple),
  roles: Vector[Role] @@ (Init, UpdCol, Unchecked),
  phone: Phone @@ (Init, Upd, Unchecked)
)

object MyData extends HKDCrudCompanion[MyData]
```
Then you get data types for reading, creating (tag `Init`) and updating (tags with prefix `Upd`) data:
```
MyData.Read ~= (id: Long, name: Option[String], updated: Instant, roles: Vector[Role], phone: Phone)
MyData.Create ~= (name: Option[String], roles: Vector[Role], phone: Phone)
MyData.Update ~= (name: Option[Option[String]], updated: Instant, roles: (add: Vector[Role], delete: Vector[Role]), phone: Option[Phone])
```
For example, types `Phone` and `Role` both have raw type `String`, then raw data (tag `Unchecked`) for creating and updating:
```
MyData.RawCreate ~= (name: Option[String], roles: Vector[String], phone: String)
MyData.RawUpdate ~= (name: Option[Option[String]], updated: Instant, roles: (add: Vector[String], delete: Vector[String]), phone: Option[String])
```
WIP: autogen transformations from raw form to validated