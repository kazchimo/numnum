package ndarray

import shapeless.Nat

trait Shape {
  type S <: Product

  def shape: S
}

object Shape {
  abstract class Shape1[N1 <: Nat: SummonNat] extends Shape {
    type S = Tuple1[N1]

    def shape: S = Tuple1(SummonNat[N1].value)
  }

  abstract class Shape2[N1 <: Nat: SummonNat, N2 <: Nat: SummonNat] extends Shape {
    type S = (N1, N2)

    def shape: S = (SummonNat[N1].value, SummonNat[N2].value)
  }

  implicit def shape1[N1 <: Nat: SummonNat]: Shape1[N1]                           = new Shape1[N1] {}
  implicit def shape2[N1 <: Nat: SummonNat, N2 <: Nat: SummonNat]: Shape2[N1, N2] =
    new Shape2[N1, N2] {}
}
