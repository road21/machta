import cats.data.ValidatedNel
import cats.Applicative
import machta.ValidatedST

package object machta extends ValidatedTInstances {
  type ValidatedST[F[_]] = [x] =>> F[ValidatedNel[String, x]]
}

trait ValidatedTInstances {
  given applicativeInstance[F[_]: Applicative]: Applicative[ValidatedST[F]] =
    Applicative[F].compose[[x] =>> ValidatedNel[String, x]]
}
