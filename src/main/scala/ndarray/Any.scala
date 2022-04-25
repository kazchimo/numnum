package ndarray

import breeze.linalg.DenseMatrix

trait Any[T] {
  def apply(t: T): Boolean
}

object Any {
  def apply[T](implicit ev: Any[T]): Any[T] = ev

  def any[T](t: T)(implicit ev: Any[T]): Boolean = ev(t)

  implicit val boolAny: Any[Boolean] = identity

  implicit val intAny: Any[Int] = _ != 0

  implicit def arrayAll[T: Any]: Any[Array[T]] = _.exists(any)

  implicit def denseMatrixAny[T: Any]: Any[DenseMatrix[T]] = new Any[DenseMatrix[T]] {
    override def apply(mat: DenseMatrix[T]): Boolean = mat.size match {
      case 0 => false
      case _ =>
        mat.foreachValue(t => if (any(t)) return true)
        false
    }
  }
}
