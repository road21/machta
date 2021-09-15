package machta
package circe

import io.circe.{Decoder, HCursor, ACursor, FailedCursor, DecodingFailure}

type DecoderK[F[_]] = TypeClassK[Decoder, F]

trait DecoderInstances:
  given noValueDecoder: Decoder[NoValue] with
    final def apply(c: HCursor): Decoder.Result[NoValue] = tryDecode(c)

    final override def tryDecode(c: ACursor): Decoder.Result[NoValue] = c match {
      case c: HCursor => Right(NoValue)
      case c: FailedCursor =>
        if (!c.incorrectFocus) Right(NoValue)
        else Left(DecodingFailure("NoValue", c.history))
    }

  given decoderKInitMWithTag[T](using C: T Contains Init): DecoderK[[x] =>> InitM[x, T]] with
    def provide[A](using dec: Decoder[A]): Decoder[InitM[A, T]] =
      new Decoder[InitM[A, T]] {
        def apply(hCursor: HCursor): Decoder.Result[InitM[A, T]] =
          dec(hCursor).map(_.asInstanceOf[InitM[A, T]])
      }

  given decoderKInitMNoTag[T](using C: T NotContains Init): DecoderK[[x] =>> InitM[x, T]] with
    def provide[A](using dec: Decoder[A]): Decoder[InitM[A, T]] =
      new Decoder[InitM[A, T]] {
        def apply(hCursor: HCursor): Decoder.Result[InitM[A, T]] = Right(NoValue.asInstanceOf[InitM[A, T]])
      }
