package machta
package circe

import syntax.no
import org.scalatest._
import flatspec._
import matchers._

import io.circe.{Decoder, Encoder}
import io.circe.parser.parse

class DecodingTest extends AnyFlatSpec with should.Matchers with EncoderInstances with DecoderInstances with CirceInstances:
  import cats.catsInstancesForId

  type F[A] = ValidatedNT[[x] =>> x, Error][A]
  given validationSvc: ValidationService[[x] =>> x] = ValidationService.instance

  given validPhone: Validatable.Aux[F, Phone, String] = Phone.valid[[x] =>> x]
  given validRole: Validatable.Aux[F, Role, String] = Role.valid[[x] =>> x]


  it should "decode valid jsons for creation successfully" in {
    val name = "Paul"
    val vipRole = "vip"
    val adminRole = "admin"
    val phone = "2128506"

    val jsonCreate = s"""{ "name": "$name", "roles": ["$vipRole", "$adminRole"], "phone": "$phone"}""".stripMargin
    val userCreate = new User.Create(NoValue, Some(name), Vector(vipRole, adminRole).map(Role.unsafeApply), Phone.unsafeApply(phone))

    val jsonRawCreate = s"""{ "name": "$name", "roles": ["$vipRole", "$adminRole"], "phone": "$phone"}""".stripMargin
    val userRawCreate = new User.RawCreate[F](
      NoValue,
      Some(name),
      Raw[Vector[Role]][F](Vector(vipRole, adminRole)),
      Raw[Phone](phone)
    )

    parse(jsonCreate).flatMap(Decoder.derived[User.Create].decodeJson) should be (Right(userCreate))
    parse(jsonRawCreate).flatMap(Decoder.derived[User.RawCreate[F]].decodeJson) should be (Right(userRawCreate))
  }
