package ndarray

import ndarray.NdArray.{NdArray1, NdArray2, NdArrayT1}
import ndarray.Shape.{Shape1, TShape1}
import ndarray.ValidShape.{ValidShape1, ValidShape2, ValidTShape1}
import shapeless.Nat

trait Transpose[S] {
  type Out

  def apply(s: S): Out
}

object Transpose {
  type Aux[S, Out0] = Transpose[S] { type Out = Out0 }

  implicit def shape1Transpose[N1 <: Nat: TShape1: ValidTShape1, T]
    : Aux[NdArray1[T, N1], NdArrayT1[T, N1]] = new Transpose[NdArray1[T, N1]] {
    type Out = NdArrayT1[T, N1]

    override def apply(s: NdArray1[T, N1]): Out = NdArray(s.values.t)
  }

  implicit def shape1Transpose[N1 <: Nat: Shape1: ValidShape1, T]
    : Aux[NdArrayT1[T, N1], NdArray1[T, N1]] = new Transpose[NdArrayT1[T, N1]] {
    type Out = NdArray1[T, N1]

    override def apply(s: NdArrayT1[T, N1]): Out = NdArray(s.values.t)
  }

  implicit def shape2Transpose[N1 <: Nat, N2 <: Nat, T](implicit
    vs: ValidShape2[N2, N1]
  ): Aux[NdArray2[T, N1, N2], NdArray2[T, N2, N1]] = new Transpose[NdArray2[T, N1, N2]] {
    type Out = NdArray2[T, N2, N1]

    override def apply(s: NdArray2[T, N1, N2]): Out = NdArray(s.values.t)
  }

}
