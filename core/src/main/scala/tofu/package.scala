import cats.effect.Bracket
import cats.{ApplicativeError, MonadError}

package object tofu {
  type HasContext[F[_], C]    = Context[F] { type Ctx    = C }
  type HasLocal[F[_], C]      = Local[F] { type Ctx      = C }
  type HasContextRun[F[_], C] = RunContext[F] { type Ctx = C }

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]
  type BracketThrow[F[_]]     = Bracket[F, Throwable]
}
