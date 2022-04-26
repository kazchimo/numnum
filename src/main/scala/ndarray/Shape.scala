package ndarray

import ndarray.Shape.{applySummonNat, sumNat}
import shapeless.Nat._1
import shapeless.ops.hlist.{LeftFolder, Length, LiftAll, Mapper}
import shapeless.ops.nat.Prod
import shapeless.{::, HList, HNil, Nat, Poly1, Poly2}

trait Shape {
  type ShapeHL <: HList

  def shape[Instances <: HList](implicit
    liftAll: LiftAll.Aux[SummonNat, ShapeHL, Instances],
    mapper: Mapper.Aux[applySummonNat.type, Instances, ShapeHL]
  ): ShapeHL = liftAll.instances.map(applySummonNat)

  def ndim[Len <: Nat](implicit ndim: Ndim.Aux[ShapeHL, Len]): Len = ndim()

  def headSize[Head <: Nat](implicit headSize: HeadSize.Aux[ShapeHL, Head]): Int = headSize.int

  def tailShape[Tail <: Shape](implicit tailShape: TailShape.Aux[ShapeHL, Tail]): Tail = tailShape()

  def isShapeOf[N <: Nat, Len <: Nat](
    n: N
  )(implicit len: Length.Aux[ShapeHL, Len], summon: SummonNat[Len]): Boolean = ndim == n

  def size[Instances <: HList, Result <: Nat](implicit
    fold: LeftFolder.Aux[ShapeHL, _1, sumNat.type, Result],
    summon: SummonNat[Result]
  ): Result = summon.value
}

object Shape {
  def of1[N1 <: Nat]: Shape1[N1]                = Shape1[N1]()
  def of2[N1 <: Nat, N2 <: Nat]: Shape2[N1, N2] = Shape2[N1, N2]()

  object applySummonNat extends Poly1 {
    implicit def caseHasSummonNat[N <: Nat]: Case.Aux[SummonNat[N], N] = at(_.value)
  }

  object sumNat extends Poly2 {
    implicit def caseNat[N1 <: Nat, N2 <: Nat, N3 <: Nat](implicit
      prod: Prod.Aux[N1, N2, N3],
      summon: SummonNat[N3]
    ): Case.Aux[N1, N2, N3] = at((_, _) => SummonNat[N3].value)
  }

  case class Shape1[N1 <: Nat]() extends Shape {
    type ShapeHL = N1 :: HNil
  }

  case class Shape2[N1 <: Nat, N2 <: Nat]() extends Shape {
    type ShapeHL = N1 :: N2 :: HNil
  }

  implicit def shape1[N1 <: Nat]: Shape1[N1]                = new Shape1[N1]
  implicit def shape2[N1 <: Nat, N2 <: Nat]: Shape2[N1, N2] = new Shape2[N1, N2]
}
