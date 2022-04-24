package ndarray

case class NdArray[T, Shape] private (value: Array[T]) {
}

object NdArray {
  trait NonNothing[T]

  object NonNothing extends NonNothing[Nothing] {
    implicit def toNonNothing[T]: NonNothing[T] = new NonNothing[T] {}

    implicit def ambiguous1: NonNothing[Nothing] = this
    implicit def ambiguous2: NonNothing[Nothing] = this
  }

  class ApplyPartiallyApplied[Shape] {
    def apply[T](value: Array[T])(implicit ev: NonNothing[Shape]): NdArray[T, Shape] = new NdArray[T, Shape](value)
  }

  def array[Shape] = new ApplyPartiallyApplied[Shape]
}
