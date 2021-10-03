package machta
package circe

import io.circe.{Json, Encoder}
import io.circe.syntax.*

type EncoderK[F[_]] = TypeClassK[Encoder, F]

trait EncoderInstances extends EncoderInstances0:
  given encoderKWithTag[T](using T Contains Init): EncoderK[[x] =>> InitM[x, T]] with
    def provide[A](using enc: Encoder[A]): Encoder[InitM[A, T]] =
      new Encoder[InitM[A, T]] {
        def apply(value: InitM[A, T]): Json = enc(value.asInstanceOf[A])
      }

  given encoderKNoTag[T](using T NotContains Init): EncoderK[[x] =>> InitM[x, T]] with
    def provide[A](using enc: Encoder[A]): Encoder[InitM[A, T]] =
      new Encoder[InitM[A, T]] {
        def apply(value: InitM[A, T]): Json = NoValue.asJson
      }

trait EncoderInstances0:
  given noValueEncoder: Encoder[NoValue] with
    def apply(a: NoValue): Json = Json.Null

  given encoderRaw[F[_], x](using V: Validatable[F, x])(using e: Encoder[V.Raw]): Encoder[Raw.Aux[F, x, V.Raw]] with
    def apply(a: Raw.Aux[F, x, V.Raw]): Json = e(a.value)