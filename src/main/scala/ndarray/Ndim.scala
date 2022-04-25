package ndarray

import shapeless.ops.hlist.Length
import shapeless.{HList, HNil, Nat}

trait Ndim[L] {
  type Out <: Nat

  def apply(): Out
}

object Ndim {
  type Aux[L, Out0 <: Nat] = Ndim[L] { type Out = Out0 }

  implicit def hnilAmbiguousNdim1: Ndim[HNil] = new Ndim[HNil] {
    type Out = Nat._0
    def apply(): Out = Nat._0
  }
  implicit def hnilAmbiguousNdim2: Ndim[HNil] = new Ndim[HNil] {
    type Out = Nat._0
    def apply(): Out = Nat._0
  }

  implicit def hlistNdim[L <: HList, Len <: Nat](implicit
    len: Length.Aux[L, Len],
    summon: SummonNat[Len]
  ): Aux[L, Len] = new Ndim[L] {
    type Out = Len

    def apply(): Out = summon.value
  }
}
