package ndarray

trait Any[T] {
  def apply(t: T): Boolean
}

object Any {
  def apply[T](implicit ev: Any[T]): Any[T] = ev

  def any[T](t: T)(implicit ev: Any[T]): Boolean = ev(t)

  implicit val boolAny: Any[Boolean] = identity

  implicit val intAny: Any[Int] = _ != 0

  implicit def arrayAll[T: Any]: Any[Array[T]] = _.exists(any)
}
