package ndarray

import breeze.linalg.DenseMatrix
import ndarray.Shape.{Shape1, Shape2, TShape1}
import shapeless.Nat
import shapeless.ops.nat.ToInt

trait ValidShape[S] {
  def apply[T](d: DenseMatrix[T]): Boolean
}

object ValidShape {
  type ValidShape1[N <: Nat]             = ValidShape[Shape1[N]]
  type ValidTShape1[N <: Nat]            = ValidShape[TShape1[N]]
  type ValidShape2[N1 <: Nat, N2 <: Nat] = ValidShape[Shape2[N1, N2]]

  implicit def shape1Valid[N1 <: Nat: ToInt]: ValidShape[Shape1[N1]] = new ValidShape[Shape1[N1]] {
    def apply[T](d: DenseMatrix[T]): Boolean = d.rows == 1 && d.cols == ToInt[N1].apply
  }

  implicit def shape2Valid[N1 <: Nat: ToInt, N2 <: Nat: ToInt]: ValidShape[Shape2[N1, N2]] =
    new ValidShape[Shape2[N1, N2]] {
      def apply[T](d: DenseMatrix[T]): Boolean =
        d.rows == ToInt[N1].apply && d.cols == ToInt[N2].apply
    }
}
