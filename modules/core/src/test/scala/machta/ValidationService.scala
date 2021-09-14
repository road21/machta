package machta

import cats.Applicative
import cats.syntax.applicative.*
import machta.{Phone, Role}
import ValidationService.{RoleError, PhoneError}

trait ValidationService[F[_]]:
  def phone(raw: String): F[Either[PhoneError, Phone]]
  def role(raw: String): F[Either[RoleError, Role]]

object ValidationService:
  def instance[F[_]: Applicative]: ValidationService[F] =
    new ValidationService[F] {
      def phone(raw: String): F[Either[PhoneError, Phone]] =
        Either.cond(raw.forall(_.isDigit), Phone.unsafeApply(raw), PhoneError(raw, "phone must contain only digits")).pure[F]

      def role(raw: String): F[Either[RoleError, Role]] =
        Either.cond(raw.forall(_.isLetter), Role.unsafeApply(raw), RoleError(raw, "role must contain only letters")).pure[F]
    }

  final case class PhoneError(input: String, msg: String)
  final case class RoleError(input: String, msg: String)