package ndarray

import breeze.linalg.DenseMatrix
import ndarray.Shape.{AmbiguousShape, Shape1, Shape2}
import shapeless.Nat
import shapeless.ops.nat.ToInt

trait ValidShape[S] {
  def apply[T](d: DenseMatrix[T]): Boolean
}

object ValidShape {
  implicit def shape1Valid[N1 <: Nat: ToInt]: ValidShape[Shape1[N1]] = new ValidShape[Shape1[N1]] {
    def apply[T](d: DenseMatrix[T]): Boolean = d.rows == 1 && d.cols == ToInt[N1].apply
  }

  implicit def shape2Valid[N1 <: Nat: ToInt, N2 <: Nat: ToInt]: ValidShape[Shape2[N1, N2]] =
    new ValidShape[Shape2[N1, N2]] {
      def apply[T](d: DenseMatrix[T]): Boolean =
        d.rows == ToInt[N1].apply && d.cols == ToInt[N2].apply
    }

  implicit def ambiguousValid: ValidShape[AmbiguousShape] = new ValidShape[AmbiguousShape] {
    def apply[T](d: DenseMatrix[T]): Boolean = true
  }
}
