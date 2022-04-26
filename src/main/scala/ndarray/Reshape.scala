package ndarray

import breeze.linalg.DenseMatrix
import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat
import shapeless.ops.nat.ToInt

trait Reshape[T, From, To <: Shape] {
  def apply(values: DenseMatrix[T]): NdArray[T, To]
}

object Reshape {
  implicit def shape1Reshape[From <: Shape, N <: Nat: ToInt, T](implicit
    sameSize: SameSize[From, Shape1[N]],
    shape: Shape1[N],
    validShape: ValidShape[Shape1[N]]
  ): Reshape[T, From, Shape1[N]] = new Reshape[T, From, Shape1[N]] {
    override def apply(values: DenseMatrix[T]): NdArray[T, Shape1[N]] =
      NdArray(values.reshape(1, ToInt[N].apply()))
  }

  implicit def shape2Reshape[From <: Shape, N1 <: Nat: ToInt, N2 <: Nat: ToInt, T](implicit
    sameSize: SameSize[From, Shape2[N1, N2]]
  ): Reshape[T, From, Shape2[N1, N2]] = new Reshape[T, From, Shape2[N1, N2]] {

    override def apply(values: DenseMatrix[T]): NdArray[T, Shape2[N1, N2]] =
      NdArray(values.reshape(ToInt[N1].apply(), ToInt[N2].apply()))
  }
}
