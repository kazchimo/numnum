package ndarray

import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat
import shapeless.ops.nat.ToInt

trait Reshape[S] {
  def column: Int
  def row: Int
}

object Reshape {
  implicit def shape1Reshape[N <: Nat: ToInt]: Reshape[Shape1[N]] = new Reshape[Shape1[N]] {
    def column: Int = ToInt[N].apply()
    def row: Int    = 1
  }

  implicit def shape2Reshape[N1 <: Nat: ToInt, N2 <: Nat: ToInt]: Reshape[Shape2[N1, N2]] =
    new Reshape[Shape2[N1, N2]] {
      def row: Int    = ToInt[N1].apply()
      def column: Int = ToInt[N2].apply()
    }
}
