package ndarray

import shapeless.ops.nat.ToInt
import shapeless.{::, HList, Nat}

trait HeadSize[L] {
  type Out

  def int: Int
}

object HeadSize {
  type Aux[L, Out0] = HeadSize[L] { type Out = Out0 }

  implicit def natHListHeadSize[H <: Nat: ToInt, Tail <: HList]: Aux[H :: Tail, H] =
    new HeadSize[H :: Tail] {
      type Out = H

      override def int: Int = ToInt[H].apply()
    }
}
