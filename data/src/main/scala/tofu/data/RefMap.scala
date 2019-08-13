package tofu.data
import tofu.data.RefMap.{ObjKey, RefKey, WitnessedKey}

final case class RefMap[+K] private (map: Map[RefKey[_], Any]) extends AnyVal {
  def put[A](objKey: ObjKey[A], value: A): RefMap[K with objKey.Tag] = RefMap(map + (objKey -> value))
  def +[A](value: A): (RefMap[K], WitnessedKey[A]) = {
    val key = new WitnessedKey[A]
    (RefMap(map + (key -> value)), key)
  }

  def apply[A](objKey: ObjKey[A])(implicit ev: K <:< objKey.Tag): A = map(objKey).asInstanceOf[A]
  def get[A](key: RefKey[A]): Option[A]                             = map.get(key).asInstanceOf[Option[A]]
}

object RefMap {
  sealed trait RefKey[A]

  trait ObjKey[A] extends RefKey[A] { self: Singleton =>
    type Tag
  }

  sealed class WitnessedKey[A] private[tofu] () extends RefKey[A]

  def initCalcWith[A, K](key: ObjKey[A], value: A): Calc[Any, RefMap[K], RefMap[K with key.Tag], Nothing, Any] =
    Calc.update(_.put(key, value))
}
