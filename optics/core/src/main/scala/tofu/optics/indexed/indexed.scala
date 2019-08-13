package tofu.optics.indexed
import cats.Monoid
import cats.arrow.Category
import tofu.optics.classes.Category2
import tofu.optics.{Optic, OpticCompanion, OpticComposeOps, OpticContext, PSame}

abstract class MonoIxOpticCompanion[PO[i, s, t, a, b] >: PSame[s, t, a, b]](
    poly: IxOpticCompanion[PO]
) {
  self =>
  type O[i, a, b] = PO[i, a, a, b, b]

  def apply[I, A, B](implicit o: O[I, A, B]): O[I, A, B] = o

  def composeIx[I, J, A, B, C](f: O[J, B, C], g: O[I, A, B]): O[(I, J), A, C]  = poly.composeIx(f, g)
  def compose[I: Monoid, A, B, C](f: O[I, B, C], g: O[I, A, B]): O[I, A, C] = poly.compose(f, g)
}

trait IxOpticCompanion[O[i, s, t, a, b] >: PSame[s, t, a, b]] {
  self =>
  type Context <: OpticContext
  type Mono[i, a, b] = O[i, a, a, b, b]

  def apply[I, S, T, A, B](implicit o: O[I, S, T, A, B]): O[I, S, T, A, B] = o

  def compose[I: Monoid, S, T, A, B, U, V](f: O[I, A, B, U, V], g: O[I, S, T, A, B]): O[I, S, T, U, V]

  def composeIx[I, J, S, T, A, B, U, V](f: O[J, A, B, U, V], g: O[I, S, T, A, B]): O[(I, J), S, T, U, V]

//  def toGeneric[S, T, A, B](o: O[S, T, A, B]): Optic[Context, S, T, A, B]
//  def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): O[S, T, A, B]

//  final implicit val category: Category[Mono] = new Category[Mono] {
//    def id[A]                                                      = PSame.id[A, A]
//    def compose[A, B, C](f: Mono[B, C], g: Mono[A, B]): Mono[A, C] = self.compose(f, g)
//  }
//
  final implicit def category2[I: Monoid]: Category2[O[I, *, *, *, *]] = new Category2[O[I, *, *, *, *]] {
    def id[A, B]: O[I, A, B, A, B]                                                            = PSame.id[A, B]
    def compose[S, T, A, B, U, V](f: O[I, A, B, U, V], g: O[I, S, T, A, B]): O[I, S, T, U, V] = self.compose(f, g)
  }

  final implicit def toOpticComposeOps[I, S, T, A, B](o: O[I, S, T, A, B]) =
    new OpticComposeOps[O[I, *, *, *, *], S, T, A, B](o)
}
