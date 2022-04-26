package ndarray

import shapeless.{::, HList, Nat}

trait HeadSize[L] {
  type Out

  def apply: Out
}

object HeadSize {
  type Aux[L, Out0] = HeadSize[L] { type Out = Out0 }

  implicit def natHListHeadSize[H <: Nat: SummonNat, Tail <: HList]: Aux[H :: Tail, H] =
    new HeadSize[H :: Tail] {
      type Out = H

      override def apply: Out = SummonNat[Out].value
    }
}
