package machta

import UpdateField.{Skip, UpdateTo}
import machta.syntax.no
import cats.data.Validated
import ValidationService.{RoleError, PhoneError}
import org.scalatest.*
import flatspec.*
import matchers.*
import ValidatableHKD.*

class ValidateTest extends AnyFlatSpec with should.Matchers:
  import cats.catsInstancesForId

  type F[A] = ValidatedNT[[x] =>> x, Error][A]
  given validationSvc: ValidationService[[x] =>> x] = ValidationService.instance

  given validPhone: Validatable.Aux[F, Phone, String] = Phone.valid[[x] =>> x]
  given validRole: Validatable.Aux[F, Role, String] = Role.valid[[x] =>> x]

  it should "validate correct init data successfully" in {
    val name = Some("John")
    val groups = Vector("admin", "vip")
    val phone = "123"

    val initU = new User.RawCreate[F](NoValue, name, Raw[Vector[Role]][F](groups), Raw[Phone](phone))
    val init = new User.Create(NoValue, name, groups.map(Role.unsafeApply), Phone.unsafeApply(phone))

    initU.validateH should be(Validated.valid(init))
  }

  it should "validate correct update data successfully" in {
    val phone = "2128506"

    val groupsAdd = Vector("lol", "kek")
    val groupsDelete = Vector("cheburek")

    val updU = new User.RawUpdate[F](
      NoValue, Skip,
      UpdateCol(
        add = Raw[Vector[Role]][F](groupsAdd),
        delete = Raw[Vector[Role]][F](groupsDelete)
      ), UpdateTo(Raw[Phone](phone))
    )

    val upd = new User.Update(
      NoValue, Skip,
      UpdateCol[Vector[Role]](
        add = groupsAdd.map(Role.unsafeApply),
        delete = groupsDelete.map(Role.unsafeApply)
      ), UpdateTo(Phone.unsafeApply(phone))
    )

    updU.validateH should be (Validated.Valid(upd))
  }

  it should "collect errors for incorrect init data" in {
    val name = Some("John")
    val groups = Vector("1", "vip", ";")
    val phone = "123"

    val initU = new User.RawCreate[F](NoValue, name, Raw[Vector[Role]][F](groups), Raw[Phone](phone))
    initU.validateH.fold(_.toList.toSet.map {
      case PhoneError(i, _) => i
      case RoleError(i, _) => i
    }, _ => Set()) should be(Set("1", ";"))
  }

  it should "collect errors for incorrect upd data" in {
    val name = None
    val groupsAdd = Vector(" ", "old")
    val groupsDelete = Vector("new", "}")
    val phone = "olol"

    val updU = new User.RawUpdate[F](
      NoValue, UpdateTo(name),
      UpdateCol(
        add = Raw[Vector[Role]][F](groupsAdd),
        delete = Raw[Vector[Role]][F](groupsDelete)
      ), UpdateTo(Raw[Phone](phone))
    )

    updU.validateH.fold(_.toList.toSet.map {
      case PhoneError(i, _) => i
      case RoleError(i, _) => i
    }, _ => Set()) should be(Set(" ", "}", phone))
  }