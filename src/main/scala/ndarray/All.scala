package ndarray

import breeze.linalg.DenseMatrix

trait All[T] { def all(t: T): Boolean }

object All {
  def apply[T](implicit ev: All[T]): All[T] = ev

  def all[T: All](t: T): Boolean = All[T].all(t)

  implicit val boolAll: All[Boolean] = identity

  implicit val intAll: All[Int] = _ != 0

  implicit def arrayAll[T: All]: All[Array[T]] = _.forall(all)

  implicit def denseMatrixAll[T: All]: All[DenseMatrix[T]] = _.forall(all(_))
}
