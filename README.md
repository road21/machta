## Machta

### Idea
Tag your hkd data class fields:

```scala
import machta.{Init, Unchecked, Upd, UpdCol, UpdReq, Data}
import machta.syntax.no
import YourCustomTypes.{Phone, Role}
import java.time.Instant

case class User[tags[_, _]](
  id: Long no tags,
  name: Option[String] tags (Init, Upd),
  updated: Instant tags UpdReq,
  roles: Vector[Role] tags (Init, UpdCol, Unchecked),
  phone: Phone tags (Init, Upd, Unchecked)
)

object User extends Data[MyData]
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
