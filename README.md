## Machta

### Idea
Tag your hkd data class fields:

```scala
import machta.{Init, Unchecked, Upd, UpdCol, UpdReq, DataCompanion}
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

object User extends DataCompanion[MyData]
```
Then you get data types for reading, creating (tag `Init`) and updating (tags with prefix `Upd`) data:
```
User.Read ~= (id: Long, name: Option[String], updated: Instant, roles: Vector[Role], phone: Phone)
User.Create ~= (name: Option[String], roles: Vector[Role], phone: Phone)
User.Update ~= (name: Option[Option[String]], updated: Instant, roles: (add: Vector[Role], delete: Vector[Role]), phone: Option[Phone])
```
For example, types `Phone` and `Role` both have raw type `String`, then raw data (tag `Unchecked`) for creating and updating:
```
User.RawCreate[F] ~= (name: Option[String], roles: Vector[String], phone: String)
User.RawUpdate[F] ~= (name: Option[Option[String]], updated: Instant, roles: (add: Vector[String], delete: Vector[String]), phone: Option[String])

val user: User.RawCreate[F] = ...
user.validate // F[User.Create]
```

### Related projects
- https://github.com/Michaelt293/higher-kinded-data
- https://github.com/scalalandio/ocdquery
- https://github.com/goosedb/HKD-Example (https://habr.com/ru/post/578070/)