package ndarray

import ndarray.Shape.{Shape1, Shape2}
import shapeless.Nat
import shapeless.ops.hlist.Length
import shapeless.ops.nat.{Diff, Div, ToInt}

case class NdArray[T, S <: Shape] private (values: Array[T])(implicit s: S) {
  def size: Int = values.length

  def ndim[Len <: Nat](implicit len: Length.Aux[s.S, Len]) = s.ndim[Len]

  def shape(implicit shape: S): shape.S = shape.shape

  def all(implicit all: All[Array[T]]): Boolean = all.all(values)

  def any(implicit any: Any[Array[T]]): Boolean = any(values)
}

object NdArray {
  trait NonNothing[T]

  object NonNothing extends NonNothing[Nothing] {
    implicit def nonNothing[T]: NonNothing[T] = new NonNothing[T] {}

    implicit val ambiguous1: NonNothing[Nothing] = this
    implicit val ambiguous2: NonNothing[Nothing] = this
  }

  class ApplyPartiallyApplied[S <: Shape] {
    def apply[T](value: Array[T])(implicit ev: NonNothing[S], s: S): NdArray[T, S] =
      new NdArray[T, S](value)
  }

  def array[S <: Shape] = new ApplyPartiallyApplied[S]

  def array1[N1 <: Nat]: ApplyPartiallyApplied[Shape1[N1]]                = array[Shape1[N1]]
  def array2[N1 <: Nat, N2 <: Nat]: ApplyPartiallyApplied[Shape2[N1, N2]] = array[Shape2[N1, N2]]

  def arrange[N <: Nat](n: N)(implicit toInt: ToInt[n.N], s: Shape1[N]): NdArray[Int, Shape1[N]] =
    array1[N](Array.range(0, Nat.toInt(n)))

  def arrange[Start <: Nat, End <: Nat, Len <: Nat](start: Start, end: End)(implicit
    sToInt: ToInt[Start],
    eToInt: ToInt[End],
    diff: Diff.Aux[End, Start, Len],
    s: Shape1[Len]
  ): NdArray[Int, Shape1[Len]] = array1[Len](Array.range(Nat.toInt[Start], Nat.toInt[End]))

  def arrange[Start <: Nat, End <: Nat, Interval <: Nat, Len <: Nat, Size <: Nat](
    start: Start,
    end: End,
    interval: Interval
  )(implicit
    sToInt: ToInt[Start],
    eToInt: ToInt[End],
    iToInt: ToInt[Interval],
    diff: Diff.Aux[End, Start, Len],
    div: Div.Aux[Len, Interval, Size],
    s: Shape1[Size]
  ): NdArray[Int, Shape1[Size]] =
    array1(Array.range(Nat.toInt[Start], Nat.toInt[End], Nat.toInt[Interval]))
}
