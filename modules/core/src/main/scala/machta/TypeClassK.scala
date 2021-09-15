package machta

trait TypeClassK[TC[_], F[_]]:
  def provide[A: TC]: TC[F[A]]

object TypeClassK:
  given instance[TC[_], A: TC, F[_]](using tc: TypeClassK[TC, F]): TC[F[A]] = tc.provide[A]
