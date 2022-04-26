package ndarray

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.storage.Zero
import ndarray.Shape.{Shape1, Shape2}
import ndarray.ValidShape.ValidShape1
import shapeless.Nat
import shapeless.ops.nat.ToInt

import scala.reflect.ClassTag

trait ArrayConstructor1[N1 <: Nat] {
  type T

  def apply(value: Array[T]): NdArray[T, Shape1[N1]]
}

object ArrayConstructor1 {
  type Aux[N1 <: Nat, T0] = ArrayConstructor1[N1] { type T = T0 }

  implicit def arrayConstructor1[N1 <: Nat: Shape1: ValidShape1, T0]: Aux[N1, T0] {
    type T = T0
  } = new ArrayConstructor1[N1] {
    type T = T0

    override def apply(value: Array[T]): NdArray[T, Shape1[N1]] =
      new NdArray(DenseVector(value).asDenseMatrix)
  }
}

trait ArrayConstructor2[N1 <: Nat, N2 <: Nat] {
  type T

  def apply(value: Array[Array[T]]): NdArray[T, Shape2[N1, N2]]
}

object ArrayConstructor2 {
  type Aux[N1 <: Nat, N2 <: Nat, T0] = ArrayConstructor2[N1, N2] { type T = T0 }

  implicit def arrayConstructor2[N1 <: Nat, N2 <: Nat, T0](implicit
    s: Shape2[N1, N2],
    toInt1: ToInt[N1],
    toInt2: ToInt[N2],
    zero: Zero[T0],
    tag: ClassTag[T0],
    vs: ValidShape[Shape2[N1, N2]]
  ): Aux[N1, N2, T0] = new ArrayConstructor2[N1, N2] {
    type T = T0

    override def apply(value: Array[Array[T]]): NdArray[T, Shape2[N1, N2]] =
      new NdArray(DenseMatrix.create(toInt1(), toInt2(), value.flatten))

  }
}
