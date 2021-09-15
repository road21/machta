package machta
package circe

import syntax.no
import org.scalatest._
import flatspec._
import matchers._

import io.circe.{Decoder, Encoder}
import io.circe.parser.parse

final case class User[tags[_, _]](
  id: Long no tags,
  name: String tags Init
)

object User extends DataCompanion[User]

class DecodingTest extends AnyFlatSpec with should.Matchers with EncoderInstances with DecoderInstances:
  it should "dummy" in {
    println(parse("""{ "name": "lol" }""").flatMap(Decoder.derived[User.Create].decodeJson))
    println(Encoder.AsObject.derived[User.Create](new User.Create(NoValue, "Lol")))
  }