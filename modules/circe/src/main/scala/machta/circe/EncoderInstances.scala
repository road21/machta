package machta
package circe

import io.circe.{Json, Encoder}

type EncoderK[F[_]] = TypeClassK[Encoder, F]

trait EncoderInstances:
  given noValueEncoder: Encoder[NoValue] with
    def apply(a: NoValue): Json = Json.Null

  given encoderKInitM[T]: EncoderK[[x] =>> InitM[x, T]] with
    def provide[A](using enc: Encoder[A]): Encoder[InitM[A, T]] =
      new Encoder[InitM[A, T]] {
        def apply(value: InitM[A, T]): Json =
          value match {
            case NoValue => Json.Null
            case x => enc(x.asInstanceOf[A])
          }
      }