package ndarray

import shapeless.ops.hlist.Tupler
import shapeless.{::, HList, HNil, Nat}

trait Shape {
  type S <: HList

  type Tuple = Tupler[S]#Out

  def shape: S
}

object Shape {
  abstract class Shape1[N1 <: Nat: SummonNat] extends Shape {
    type S = N1 :: HNil

    def shape: S = SummonNat[N1].value :: HNil
  }

  abstract class Shape2[N1 <: Nat: SummonNat, N2 <: Nat: SummonNat] extends Shape {
    type S = N1 :: N2 :: HNil

    def shape: S = SummonNat[N1].value :: SummonNat[N2].value :: HNil
  }

  implicit def shape1[N1 <: Nat: SummonNat]: Shape1[N1]                           = new Shape1[N1] {}
  implicit def shape2[N1 <: Nat: SummonNat, N2 <: Nat: SummonNat]: Shape2[N1, N2] =
    new Shape2[N1, N2] {}
}
