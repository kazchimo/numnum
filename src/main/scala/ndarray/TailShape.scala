package ndarray

import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat

trait TailShape[Shape] {
  type Tail

  def apply(): Tail
}

object TailShape {
  type Aux[Shape, O] = TailShape[Shape] { type Tail = O }

  implicit def shape2Tail[N1 <: Nat, N2 <: Nat]: TailShape.Aux[Shape2[N1, N2], Shape1[N2]] =
    new TailShape[Shape2[N1, N2]] {
      type Tail = Shape1[N2]

      override def apply(): Tail = Shape1[N2]()
    }
}
