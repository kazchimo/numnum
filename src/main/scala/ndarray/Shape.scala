package ndarray

import shapeless.ops.nat.Prod
import shapeless.{::, HList, HNil, Nat, Poly2}

trait Shape {
  type ShapeHL <: HList

  def shape(implicit genShape: GenShape[ShapeHL]): genShape.Out = genShape.apply

  def ndim(implicit ndim: Ndim[ShapeHL]): ndim.Out = ndim()

  def headSize(implicit headSize: HeadSize[ShapeHL]): headSize.Out = headSize.apply

  def tailShape(implicit tailShape: TailShape[ShapeHL]): tailShape.Tail = tailShape()

  def isNdimOf[N <: Nat](implicit isNdimOf: IsNdimOf[ShapeHL, N]): isNdimOf.Out = isNdimOf.apply

  def size(implicit size: Size[ShapeHL]): size.Out = size.apply
}

object Shape {
  def of1[N1 <: Nat]: Shape1[N1]                = Shape1[N1]()
  def of2[N1 <: Nat, N2 <: Nat]: Shape2[N1, N2] = Shape2[N1, N2]()

  object sumNat extends Poly2 {
    implicit def caseNat[N1 <: Nat, N2 <: Nat, N3 <: Nat](implicit
      prod: Prod.Aux[N1, N2, N3],
      summon: SummonNat[N3]
    ): Case.Aux[N1, N2, N3] = at((_, _) => SummonNat[N3].value)
  }

  case class Shape1[N1 <: Nat]() extends Shape {
    type ShapeHL = N1 :: HNil
  }

  /* transposed Shape1 */
  case class TShape1[N1 <: Nat]() extends Shape {
    type ShapeHL = N1 :: HNil
  }

  case class Shape2[N1 <: Nat, N2 <: Nat]() extends Shape {
    type ShapeHL = N1 :: N2 :: HNil
  }

  implicit def shape1[N1 <: Nat]: Shape1[N1]                = new Shape1[N1]
  implicit def shape2[N1 <: Nat, N2 <: Nat]: Shape2[N1, N2] = new Shape2[N1, N2]
}
