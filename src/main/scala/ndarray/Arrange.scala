package ndarray

import ndarray.NdArray.array1
import ndarray.Shape.Shape1
import shapeless.{Nat, Succ}
import shapeless.ops.nat.{Diff, Div, ToInt}

abstract class Arrange1[N1 <: Nat: ToInt: Shape1] {
  def apply: NdArray[Int, Shape1[N1]]
}

object Arrange1 {
  def apply[N1 <: Nat](implicit ev: Arrange1[N1]): Arrange1[N1] = ev

  implicit def arrange1[N1 <: Nat: ToInt: Shape1]: Arrange1[N1] = new Arrange1[N1] {
    def apply: NdArray[Int, Shape1[N1]] = array1[N1](Array.range(0, Nat.toInt[N1]))
  }
}

abstract class Arrange2[Start <: Nat, End <: Nat] {
  type Out <: Nat

  def apply: NdArray[Int, Shape1[Out]]
}

object Arrange2 {
  type Aux[Start <: Nat, End <: Nat, Out0 <: Nat] = Arrange2[Start, End] { type Out = Out0 }

  implicit def arrange2[Start <: Nat, End <: Nat, Len <: Nat](implicit
    sToInt: ToInt[Start],
    eToInt: ToInt[End],
    diff: Diff.Aux[End, Start, Len],
    s: Shape1[Len],
    vs: ValidShape[Shape1[Len]]
  ): Aux[Start, End, Len] = new Arrange2[Start, End] {
    type Out = Len

    def apply: NdArray[Int, Shape1[Out]] =
      array1[Len](Array.range(Nat.toInt[Start], Nat.toInt[End]))
  }
}

abstract class Arrange3[Start <: Nat, End <: Nat, Step <: Nat] {
  type Out <: Nat

  def apply: NdArray[Int, Shape1[Out]]
}

object Arrange3 {
  type Aux[Start <: Nat, End <: Nat, Step <: Nat, Out0 <: Nat] =
    Arrange3[Start, End, Step] { type Out = Out0 }

  implicit def arrange3[Start <: Nat, End <: Nat, Step <: Nat, Len <: Nat, Size <: Nat](implicit
    sToInt: ToInt[Start],
    eToInt: ToInt[End],
    iToInt: ToInt[Step],
    diff: Diff.Aux[End, Start, Len],
    div: Div.Aux[Len, Step, Size],
    s: Shape1[Succ[Size]],
    vs: ValidShape[Shape1[Succ[Size]]]
  ): Aux[Start, End, Step, Succ[Size]] = new Arrange3[Start, End, Step] {
    type Out = Succ[Size]

    def apply: NdArray[Int, Shape1[Out]] =
      array1(Array.range(Nat.toInt[Start], Nat.toInt[End], Nat.toInt[Step]))
  }
}
