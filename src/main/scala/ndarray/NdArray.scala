package ndarray

case class NdArray[T, Shape] private (value: Array[T]) {}

object NdArray {
  trait NonNothing[T]

  object NonNothing extends NonNothing[Nothing] {
    implicit def toNonNothing[T]: NonNothing[T] = new NonNothing[T] {}

    implicit def ambiguous1: NonNothing[Nothing] = this
    implicit def ambiguous2: NonNothing[Nothing] = this
  }

  class ApplyPartiallyApplied[S <: Shape] {
    def apply[T](value: Array[T])(implicit ev: NonNothing[S]): NdArray[T, S] =
      new NdArray[T, S](value)
  }

  def array[S <: Shape] = new ApplyPartiallyApplied[S]
}
