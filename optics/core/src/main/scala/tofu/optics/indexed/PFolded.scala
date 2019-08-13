package tofu.optics
package indexed

import cats.instances.list._
import cats.syntax.monoid._
import cats.{Applicative, Foldable, Monoid}
import tofu.optics.data.Constant

/** S has some or none occurrences of A
  * and can collect them */
trait PIxFolded[+I, -S, +T, +A, -B] extends PFolded[S, T, A, B]{

  def indices: PFolded[S, T, I, Any] = PFolded.apply

  def foldMap[X: Monoid](s: S)(f: A => X): X = foldMapIx(s)((_, a) => f(a))
  def foldMapIx[X: Monoid](s: S)(f: (I, A) => X): X

  def getWithIndices(s: S): List[(I, A)] = foldMapIx(s)((i, x) => List((i, x)))

  override def as[B1, T1]: PIxFolded[I, S, T1, A, B1] = this.asInstanceOf[PIxFolded[I, S, T1, A, B1]]
}

object IxFolded extends MonoIxOpticCompanion(PIxFolded) {
  def apply[S] = new FoldedApply[S]

  class FoldedApply[S] {
    type Arb

    def apply[A](fm: Monoid[Arb] => (S, A => Arb) => Arb): Folded[S, A] = new Folded[S, A] {
      def foldMap[X: Monoid](s: S)(f: A => X): X =
        fm(Monoid[X].asInstanceOf[Monoid[Arb]])(s, f.asInstanceOf[A => Arb]).asInstanceOf[X]
    }
  }
}

object PIxFolded extends IxOpticCompanion[PIxFolded] {
  implicit class TofuFoldedOps[S, T, A, B](private val self: PFolded[S, T, A, B]) extends AnyVal {
    def ++[S1 <: S, T1, A1 >: A, V1](that: PFolded[S1, T1, A1, V1]): PFolded[S1, T1, A1, V1] =
      new PFolded[S1, T1, A1, V1] {
        def foldMap[X: Monoid](s: S1)(f: A1 => X): X = self.foldMap(s)(f) |+| that.foldMap(s)(f)
      }
  }

  def compose[I: Monoid, S, T, A, B, U, V](f: PIxFolded[I, A, B, U, V], g: PIxFolded[I, S, T, A, B]): PIxFolded[I, S, T, U, V] =
    new PIxFolded[I, S, T, U, V] {
      def foldMapIx[X: Monoid](s: S)(f: (I, U) => X): X =

}
  def composeIx[I, J, S, T, A, B, U, V](f: PIxFolded[J, A, B, U, V], g: PIxFolded[I, S, T, A, B]): PIxFolded[(I, J), S, T, U, V] = ???
  def compose[S, T, A, B, U, V](f: PFolded[A, B, U, V], g: PFolded[S, T, A, B]): PFolded[S, T, U, V] =
    new PFolded[S, T, U, V] {
      def foldMap[X: Monoid](s: S)(fux: U => X): X = g.foldMap(s)(f.foldMap(_)(fux))
    }


  final implicit def byFoldable[F[_], A, T, B](implicit F: Foldable[F]): PFolded[F[A], T, A, B] =
    new PFolded[F[A], T, A, B] {
      def foldMap[X: Monoid](fa: F[A])(f: A => X): X = F.foldMap(fa)(f)
    }

  trait Context extends PReduced.Context with PItems.Context with PDowncast.Context {
    override def algebra: Monoid[X]
    def default: X = algebra.empty
    override val functor: Applicative[F] = {
      implicit val alg = algebra
      Applicative[Constant[X, +*]]
    }
  }

  override def toGeneric[S, T, A, B](o: PFolded[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Constant[c.X, B]): S => Constant[c.X, T] =
        s => Constant.Impl(o.foldMap(s)(a => p(a).value)(c.algebra))
    }
  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PFolded[S, T, A, B] =
    new PFolded[S, T, A, B] {
      def foldMap[Y: Monoid](s: S)(f: A => Y): Y =
        o(new Context {
          type X = Y
          override def algebra = Monoid[Y]
        })(a => Constant(f(a)))(s).value
    }
}
