package ndarray

import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat

case class NdArray[T, S <: Shape] private (value: Array[T]) {
  def size: Int = value.length

  def ndim(implicit ndim: NDim[S]): Int = ndim.ndim

  def shape(implicit shape: S): shape.S = shape.shape

  def all(implicit all: All[Array[T]]): Boolean = all.all(value)
}

object NdArray {

  trait NonNothing[T]

  object NonNothing extends NonNothing[Nothing] {
    implicit def nonNothing[T]: NonNothing[T] = new NonNothing[T] {}

    implicit val ambiguous1: NonNothing[Nothing] = this
    implicit val ambiguous2: NonNothing[Nothing] = this
  }

  class ApplyPartiallyApplied[S <: Shape] {
    def apply[T](value: Array[T])(implicit ev: NonNothing[S]): NdArray[T, S] =
      new NdArray[T, S](value)
  }

  def array[S <: Shape] = new ApplyPartiallyApplied[S]

  def array1[N1 <: Nat]: ApplyPartiallyApplied[Shape1[N1]]                = array[Shape1[N1]]
  def array2[N1 <: Nat, N2 <: Nat]: ApplyPartiallyApplied[Shape2[N1, N2]] = array[Shape2[N1, N2]]
}
