package ndarray

import ndarray.Shape.{applySummonNat, sumNat}
import shapeless.Nat._1
import shapeless.ops.hlist.{IsHCons, LeftFolder, Length, LiftAll, Mapper}
import shapeless.ops.nat.{Prod, ToInt}
import shapeless.{::, HList, HNil, Nat, Poly1, Poly2}

trait Shape {
  type Shape <: HList

  def shape[Instances <: HList](implicit
    liftAll: LiftAll.Aux[SummonNat, Shape, Instances],
    mapper: Mapper.Aux[applySummonNat.type, Instances, Shape]
  ): Shape = liftAll.instances.map(applySummonNat)

  def ndim[Len <: Nat](implicit len: Length.Aux[Shape, Len], toInt: ToInt[Len]): Int = toInt()

  def headSize[Head <: Nat, Tail <: HList](implicit
    isH: IsHCons.Aux[Shape, Head, Tail],
    toInt: ToInt[Head]
  ): Int = toInt()

  def isShape1[Len <: Nat](implicit len: Length.Aux[Shape, Len], toInt: ToInt[Len]): Boolean =
    ndim == 1

  def size[Instances <: HList, Result <: Nat](implicit
    fold: LeftFolder.Aux[Shape, _1, sumNat.type, Result],
    summon: SummonNat[Result]
  ): Result = summon.value
}

object Shape {
  object applySummonNat extends Poly1 {
    implicit def caseHasSummonNat[N <: Nat]: Case.Aux[SummonNat[N], N] = at(_.value)
  }

  object sumNat extends Poly2 {
    implicit def caseNat[N1 <: Nat, N2 <: Nat, N3 <: Nat](implicit
      prod: Prod.Aux[N1, N2, N3],
      summon: SummonNat[N3]
    ): Case.Aux[N1, N2, N3] = at((_, _) => SummonNat[N3].value)
  }

  class Shape1[N1 <: Nat] extends Shape {
    type Shape = N1 :: HNil
  }

  class Shape2[N1 <: Nat, N2 <: Nat] extends Shape {
    type Shape = N1 :: N2 :: HNil
  }

  implicit def shape1[N1 <: Nat]: Shape1[N1]                = new Shape1[N1]
  implicit def shape2[N1 <: Nat, N2 <: Nat]: Shape2[N1, N2] = new Shape2[N1, N2]
}
