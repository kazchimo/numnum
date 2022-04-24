package ndarray

import shapeless.ops.nat.ToInt
import shapeless.{::, HList, HNil, Nat}

trait Shape {
  type S <: HList

  def shape: S
}

object Shape {
  class Shape1[N1 <: Nat: SummonNat: ToInt] extends Shape {
    type S = N1 :: HNil

    def shape: S = SummonNat[N1].value :: HNil
  }

  class Shape2[N1 <: Nat: SummonNat: ToInt, N2 <: Nat: SummonNat: ToInt] extends Shape {
    type S = N1 :: N2 :: HNil

    def shape: S = SummonNat[N1].value :: SummonNat[N2].value :: HNil
  }

  implicit def shape1[N1 <: Nat: SummonNat: ToInt]: Shape1[N1]                                  = new Shape1[N1]
  implicit def shape2[N1 <: Nat: SummonNat: ToInt, N2 <: Nat: SummonNat: ToInt]: Shape2[N1, N2] =
    new Shape2[N1, N2]
}
