package ndarray

import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

trait Full[S <: Shape] {
  type T

  def apply(t: T): NdArray[T, S]
}

object Full {
  type Aux[S <: Shape, T0] = Full[S] { type T = T0 }

  implicit def shape1[N1 <: Nat: ToInt, T0: Zero: ClassTag]: Aux[Shape1[N1], T0] =
    new Full[Shape1[N1]] {
      type T = T0

      def apply(t: T): NdArray[T, Shape1[N1]] = NdArray(DenseMatrix.fill(1, ToInt[N1].apply)(t))
    }

  implicit def shape2[N1 <: Nat: ToInt, N2 <: Nat: ToInt, T0: Zero: ClassTag]
    : Aux[Shape2[N1, N2], T0] = new Full[Shape2[N1, N2]] {
    type T = T0

    def apply(t: T): NdArray[T, Shape2[N1, N2]] =
      NdArray(DenseMatrix.fill(ToInt[N1].apply, ToInt[N2].apply)(t))
  }

}
