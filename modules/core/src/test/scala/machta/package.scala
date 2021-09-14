import cats.data.ValidatedNel
import cats.Applicative
import machta.ValidatedNT

package object machta extends ValidatedTInstances {
  type  ValidatedNT[F[_], E] = [x] =>> F[ValidatedNel[E, x]]
}

trait ValidatedTInstances {
  given applicativeInstance[E, F[_]: Applicative]: Applicative[ValidatedNT[F, E]] =
    Applicative[F].compose[[x] =>> ValidatedNel[E, x]]
}
