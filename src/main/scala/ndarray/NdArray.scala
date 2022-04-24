package ndarray

case class NdArray[T, S <: Shape] private (value: Array[T]) {
  def size: Int = value.length
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
}
