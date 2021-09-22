package machta
package circe

import io.circe.{ACursor, Decoder, DecodingFailure, FailedCursor, HCursor, Json}

type DecoderK[F[_]] = TypeClassK[Decoder, F]

trait DecoderInstances extends DecoderInstances0:
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

  given decoderKCheckNoTag[T, U[_, _], F[_]](using C: T NotContains Unchecked, D: DecoderK[[x] =>> U[x, T]]): DecoderK[[x] =>> RawForm[F, x, T, U]] with
    def provide[A](using dec: Decoder[A]): Decoder[RawForm[F, A, T, U]] =
      new Decoder[RawForm[F, A, T, U]] {
        def apply(hCursor: HCursor): Decoder.Result[RawForm[F, A, T, U]] =
          D.provide[A].apply(hCursor).asInstanceOf
      }

  given decoderCheckTag[T, U[_, _], F[_], X](using C: T Contains Unchecked, D: DecoderK[[x] =>> U[x, T]], V: Validatable[F, X], d: Decoder[V.Raw]): Decoder[RawForm[F, X, T, U]] with
    def apply(hCursor: HCursor): Decoder.Result[RawForm[F, X, T, U]] =
      D.provide[Raw[F, X]].apply(hCursor).asInstanceOf

trait DecoderInstances0:
  given decoderRaw[F[_], x](using V: Validatable[F, x], d: Decoder[V.Raw]): Decoder[Raw[F, x]] with
    def apply(hCursor: HCursor): Decoder.Result[Raw[F, x]] =
      d(hCursor).map(r => Raw[x](r))

  given noValueDecoder: Decoder[NoValue] with
    final def apply(c: HCursor): Decoder.Result[NoValue] = tryDecode(c)

    final override def tryDecode(c: ACursor): Decoder.Result[NoValue] = c match {
      case c: HCursor => c.value match {
        case other if other.isNull            => Right(NoValue)
        case _                                => Left(DecodingFailure("NoValue", c.history))
      }
      case c: FailedCursor =>
        if (!c.incorrectFocus) Right(NoValue)
        else Left(DecodingFailure("NoValue", c.history))
    }