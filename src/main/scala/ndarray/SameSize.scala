package ndarray

import ndarray.Shape.sumNat
import shapeless.Nat
import shapeless.Nat._1
import shapeless.ops.hlist.LeftFolder

import scala.annotation.implicitNotFound

@implicitNotFound("${S1} and ${S2} must have the same size")
trait SameSize[S1, S2]

object SameSize extends SameSize[scala.Any, scala.Any] {
  implicit def sameSizeShape[S1 <: Shape, S2 <: Shape, SizeOfTo <: Nat, SizeOfFrom <: Nat](implicit
    fold1: LeftFolder.Aux[S1#ShapeHL, _1, sumNat.type, SizeOfTo],
    fold2: LeftFolder.Aux[S2#ShapeHL, _1, sumNat.type, SizeOfFrom],
    sameShape: SizeOfFrom =:= SizeOfTo
  ): SameSize[S1, S2] = this.asInstanceOf[SameSize[S1, S2]]
}
