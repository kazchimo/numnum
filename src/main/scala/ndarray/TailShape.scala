package ndarray

import ndarray.Shape.Shape1
import shapeless.{HNil, Nat, ::}

trait TailShape[Shape] {
  type Tail

  def apply(): Tail
}

object TailShape {
  type Aux[Shape, O] = TailShape[Shape] { type Tail = O }

  implicit def shape2Tail[N1 <: Nat, N2 <: Nat]: TailShape.Aux[N1 :: N2 :: HNil, Shape1[N2]] =
    new TailShape[N1 :: N2 :: HNil] {
      type Tail = Shape1[N2]

      override def apply(): Tail = Shape1[N2]()
    }
}
