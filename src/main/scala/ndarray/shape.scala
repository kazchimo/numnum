package ndarray

object shape {
  type Shape0 = Unit
  type Shape1[T <: Int with Singleton] = T
}
