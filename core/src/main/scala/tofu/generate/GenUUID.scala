package tofu.generate

import java.util.UUID

import cats.Functor
import cats.effect.Sync
import simulacrum.typeclass
import cats.syntax.functor._
import cats.tagless.ApplyK

@typeclass
trait GenUUID[F[_]] {
  def randomUUID: F[UUID]
}

object GenUUID {
  def random[F[_]](implicit gen: GenUUID[F]): F[UUID] = gen.randomUUID
  def randomString[F[_]: Functor: GenUUID]: F[String] = random[F].map(_.toString)

  implicit def syncGenUUID[F[_]](implicit F: Sync[F]): GenUUID[F] = new GenUUID[F] {
    val randomUUID: F[UUID] = F.delay(UUID.randomUUID())
  }

  implicit val applyK: ApplyK[GenUUID] = cats.tagless.Derive.applyK
}