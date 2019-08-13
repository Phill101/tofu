package tofu
package errorInstances

import cats.ApplicativeError
import tofu.optics.{Downcast, Upcast}

import scala.reflect.ClassTag

private[tofu] class FromAppErr[F[_], E, E1](
    implicit
    protected val appErr: ApplicativeError[F, E],
    protected val sub: E1 <:< E
)

private[tofu] trait RaiseApErr[F[_], E, E1] extends Raise[F, E1] {
  self: FromAppErr[F, E, E1] =>
  def raise[A](err: E1): F[A] = appErr.raiseError(err)
}

private[tofu] class HandleApErr[F[_]: ApplicativeError[*[_], E], E, E1: ClassTag: * <:< E]
    extends FromAppErr[F, E, E1] with Handle.ByRecover[F, E1] with OnError[F, E1] {

  def onErrorTry[A, B](fa: F[A])(f: E1 => Option[F[B]]): F[A] = ???

  override def onError[A, B](fa: F[A])(f: E1 => F[B]): F[A] = appErr.onError(fa) { case e: E1 => appErr.void(f(e)) }

  override def onErrorPF[A, B](fa: F[A])(f: PartialFunction[E1, F[B]]): F[A] =
    appErr.onError(fa) { case e: E1 if f.isDefinedAt(e) => appErr.void(f(e)) }

  def recWith[A](fa: F[A])(pf: PartialFunction[E1, F[A]]): F[A] =
    appErr.recoverWith(fa)({ case e1: E1 if pf.isDefinedAt(e1) => pf(e1) })

  def restore[A](fa: F[A]): F[Option[A]] = appErr.handleError[Option[A]](appErr.map(fa)(Some(_)))(_ => None)
}

private[tofu] class FromPrism[F[_], E, E1, +TC[_[_], _], +P[_, _]](
    implicit
    protected val instance: TC[F, E],
    protected val prism: P[E, E1]
)

private[tofu] trait RaisePrism[F[_], E, E1] extends Raise[F, E1] {
  self: FromPrism[F, E, E1, Raise, Upcast] =>

  def raise[A](err: E1): F[A] = instance.raise(prism.upcast(err))
}

private[tofu] trait HandlePrism[F[_], E, E1] extends Handle[F, E1] {
  self: FromPrism[F, E, E1, Handle, Downcast] =>

  def tryHandleWith[A](fa: F[A])(f: E1 => Option[F[A]]): F[A] =
    instance.tryHandleWith(fa)(e => prism.downcast(e).flatMap(f))

  def restore[A](fa: F[A]): F[Option[A]] = instance.restore(fa)
}

private[tofu] trait OnErrorPrism[F[_], E, E1] extends OnError[F, E1] {
  self: FromPrism[F, E, E1, OnError, Downcast] =>

  def onErrorTry[A, B](fa: F[A])(f: E1 => Option[F[B]]): F[A] =
    instance.onErrorTry(fa)(e => prism.downcast(e).flatMap(f))
}
