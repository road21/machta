package machta

import cats.data.ValidatedNel
import cats.Functor
import cats.syntax.functor.*
import cats.syntax.either.*

type Error = ValidationService.PhoneError | ValidationService.RoleError

opaque type Phone = String
object Phone:
  def unsafeApply(str: String): Phone = str
  implicit def valid[F[_]: Functor](using v: ValidationService[F]): Validatable.Aux[ValidatedNT[F, Error], Phone, String] =
    new Validatable[ValidatedNT[F, Error], Phone] {
      type Raw = String
      def validate(r: Raw): F[ValidatedNel[Error, Phone]] = v.phone(r).map(_.toValidatedNel)
    }

  extension (p: Phone) def toString: String = p

opaque type Role = String
object Role:
  def unsafeApply(str: String): Role = str
  implicit def valid[F[_]: Functor](using v: ValidationService[F]): Validatable.Aux[ValidatedNT[F, Error], Role, String] =
    new Validatable[ValidatedNT[F, Error], Role] {
      type Raw = String

      def validate(r: Raw): F[ValidatedNel[Error, Role]] = v.role(r).map(_.toValidatedNel)
    }
  extension (p: Role) def toString: String = p