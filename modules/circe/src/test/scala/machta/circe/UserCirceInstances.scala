package machta
package circe

import io.circe.{Encoder, Decoder}

trait UserCirceInstances:
  given encoderPhone: Encoder[Phone] = Encoder[String].contramap(x => x.toString)
  given decoderPhone: Decoder[Phone] = Decoder[String].map(Phone.unsafeApply)
  given encoderRole: Encoder[Role] = Encoder[String].contramap(x => x.toString)
  given decoderRole: Decoder[Role] = Decoder[String].map(Role.unsafeApply)
