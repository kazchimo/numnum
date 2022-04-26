package ndarray

import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

trait Full[S <: Shape] {
  def apply[T: Zero: ClassTag](t: T): NdArray[T, S]
}

object Full {
  def apply[S <: Shape](implicit ev: Full[S]): Full[S] = ev

  implicit def shape1[N1 <: Nat: ToInt]: Full[Shape1[N1]] = new Full[Shape1[N1]] {
    def apply[T: Zero: ClassTag](t: T): NdArray[T, Shape1[N1]] =
      NdArray(DenseMatrix.fill(1, ToInt[N1].apply)(t))
  }

  implicit def shape2[N1 <: Nat: ToInt, N2 <: Nat: ToInt]: Full[Shape2[N1, N2]] =
    new Full[Shape2[N1, N2]] {
      def apply[T: Zero: ClassTag](t: T): NdArray[T, Shape2[N1, N2]] =
        NdArray(DenseMatrix.fill(ToInt[N1].apply, ToInt[N2].apply)(t))
    }

}
