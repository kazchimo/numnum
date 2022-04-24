package ndarray

case class NdArray[T, Shape] private (value: Array[T]) {}

object NdArray {

  class ApplyPartiallyApplied[S <: Shape] {
    def apply[T](value: Array[T]): NdArray[T, S] = new NdArray[T, S](value)
  }

  def array[S <: Shape] = new ApplyPartiallyApplied[S]
}
