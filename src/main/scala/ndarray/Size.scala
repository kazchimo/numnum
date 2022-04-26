package ndarray

import ndarray.Shape.sumNat
import shapeless.Nat._1
import shapeless.ops.hlist.LeftFolder
import shapeless.{HList, Nat}

trait Size[S <: HList] {
  type Out <: Nat

  def apply: Out
}

object Size {
  type Aux[S <: HList, Out0 <: Nat] = Size[S] { type Out = Out0 }

  implicit def size[S <: HList, Instances <: HList, Result <: Nat](implicit
    fold: LeftFolder.Aux[S, _1, sumNat.type, Result],
    summon: SummonNat[Result]
  ): Aux[S, Result] = new Size[S] {
    type Out = Result

    def apply: Out = summon.value
  }
}
