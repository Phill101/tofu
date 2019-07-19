package tofu
package logging
package impl

import cats.FlatMap
import cats.syntax.flatMap._
import org.slf4j.Marker

class EmbedLogging[F[_]: FlatMap](underlying : F[Logging[F]]) extends Logging[F] {
  def write(level: Logging.Level, message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.write(level, message, values))

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.writeMarker(level, message, marker, values: _*))
  override def writeCause(level: Logging.Level, message: String, cause: Throwable): F[Unit] =
    underlying.flatMap(_.writeCause(level, message, cause))
  override def trace(message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.trace(message, values: _*))
  override def debug(message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.debug(message, values: _*))
  override def info(message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.info(message, values: _*))
  override def warn(message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.warn(message, values: _*))
  override def error(message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.error(message, values: _*))
  override def traceWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.traceWithMarker(message, marker, values))
  override def debugWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.debugWithMarker(message, marker, values))
  override def infoWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.infoWithMarker(message, marker, values))
  override def warnWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.warnWithMarker(message, marker, values))
  override def errorWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.errorWithMarker(message, marker, values))
  override def traceCause(message: String, cause: Throwable): F[Unit] = underlying.flatMap(_.traceCause(message, cause))
  override def debugCause(message: String, cause: Throwable): F[Unit] = underlying.flatMap(_.debugCause(message, cause))
  override def infoCause(message: String, cause: Throwable): F[Unit]  = underlying.flatMap(_.infoCause(message, cause))
  override def warnCause(message: String, cause: Throwable): F[Unit]  = underlying.flatMap(_.warnCause(message, cause))
  override def errorCause(message: String, cause: Throwable): F[Unit] = underlying.flatMap(_.errorCause(message, cause))
}
