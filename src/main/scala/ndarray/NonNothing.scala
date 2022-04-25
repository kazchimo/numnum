package ndarray

trait NonNothing[T]

object NonNothing extends NonNothing[Nothing] {
  implicit def nonNothing[T]: NonNothing[T] = new NonNothing[T] {}

  implicit val ambiguous1: NonNothing[Nothing] = this
  implicit val ambiguous2: NonNothing[Nothing] = this
}
