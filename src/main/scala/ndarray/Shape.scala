package ndarray

import ndarray.Shape.applySummonNat
import shapeless.ops.hlist.{IsHCons, Length, LiftAll, Mapper}
import shapeless.ops.nat.ToInt
import shapeless.{::, HList, HNil, Nat, Poly1}

trait Shape {
  type S <: HList

  def shape[Instances <: HList](implicit
    liftAll: LiftAll.Aux[SummonNat, S, Instances],
    mapper: Mapper.Aux[applySummonNat.type, Instances, S]
  ): S = liftAll.instances.map(applySummonNat)

  def ndim[Len <: Nat](implicit len: Length.Aux[S, Len], toInt: ToInt[Len]): Int = toInt()

  def headSize[Head <: Nat, Tail <: HList](implicit
    isH: IsHCons.Aux[S, Head, Tail],
    toInt: ToInt[Head]
  ): Int = toInt()

  def isShape1[Len <: Nat](implicit len: Length.Aux[S, Len], toInt: ToInt[Len]): Boolean = ndim == 1

//  def validShape[T, Head <: Nat, Tail <: HList](
//    arr: Array[T]
//  )(implicit isH: IsHCons.Aux[S, Head, Tail], toInt: ToInt[Head]) = arr.size == headSize
}

object Shape {
  object applySummonNat extends Poly1 {
    implicit def caseHasSummonNat[N <: Nat]: Case.Aux[SummonNat[N], N] = at[SummonNat[N]](_.value)
  }

  class Shape1[N1 <: Nat: SummonNat: ToInt] extends Shape {
    type S = N1 :: HNil
  }

  class Shape2[N1 <: Nat: SummonNat: ToInt, N2 <: Nat: SummonNat: ToInt] extends Shape {
    type S = N1 :: N2 :: HNil
  }

  implicit def shape1[N1 <: Nat: SummonNat: ToInt]: Shape1[N1]                                  = new Shape1[N1]
  implicit def shape2[N1 <: Nat: SummonNat: ToInt, N2 <: Nat: SummonNat: ToInt]: Shape2[N1, N2] =
    new Shape2[N1, N2]
}
