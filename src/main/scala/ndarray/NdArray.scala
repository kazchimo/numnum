package ndarray

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.storage.Zero
import ndarray.Shape.{Shape1, Shape2, applySummonNat, sumNat}
import shapeless.Nat._1
import shapeless.ops.hlist.{LeftFolder, Length, LiftAll, Mapper}
import shapeless.ops.nat.{Diff, Div, ToInt}
import shapeless.{HList, Nat, Succ}

import scala.reflect.ClassTag

case class NdArray[T, S <: Shape] private (values: DenseMatrix[T])(implicit
  val s: S,
  validShape: ValidShape[S]
) {
  require(validShape(values))

  def length: Int = values.size

  def toArray: Array[T] = values.toArray

  def ndim[Len <: Nat](implicit len: Length.Aux[s.ShapeHL, Len], summonNat: SummonNat[Len]): Len =
    s.ndim

  def shape[Instances <: HList](implicit
    liftAll: LiftAll.Aux[SummonNat, s.ShapeHL, Instances],
    mapper: Mapper.Aux[applySummonNat.type, Instances, s.ShapeHL]
  ): s.ShapeHL = s.shape

  def all(implicit all: All[DenseMatrix[T]]): Boolean = all.all(values)

  def any(implicit any: Any[DenseMatrix[T]]): Boolean = any(values)

  def size[Instances <: HList, Result <: Nat](implicit
    fold: LeftFolder.Aux[s.ShapeHL, _1, sumNat.type, Result],
    summon: SummonNat[Result]
  ): Result = s.size

  def reshape[ToShape <: Shape, Head <: Nat](shape: ToShape)(implicit
    sameSize: SameSize[S, ToShape],
    res: Reshape[ToShape],
    validShape: ValidShape[ToShape]
  ): NdArray[T, ToShape] = NdArray(values.reshape(res.row, res.column))(shape, validShape)
}

object NdArray {

  class Array1PartiallyApplied[N1 <: Nat] {
    def apply[T](
      value: Array[T]
    )(implicit s: Shape1[N1], vs: ValidShape[Shape1[N1]]): NdArray[T, Shape1[N1]] =
      new NdArray(DenseVector(value).asDenseMatrix)
  }

  class Array2PartiallyApplied[N1 <: Nat, N2 <: Nat] {
    def apply[T, O1 <: Nat, O2 <: Nat](value: Array[Array[T]])(implicit
      s: Shape2[N1, N2],
      toInt1: ToInt[N1],
      toInt2: ToInt[N2],
      zero: Zero[T],
      tag: ClassTag[T],
      vs: ValidShape[Shape2[N1, N2]]
    ): NdArray[T, Shape2[N1, N2]] =
      new NdArray(DenseMatrix.create(toInt1(), toInt2(), value.flatten))
  }

  def array1[N1 <: Nat]: Array1PartiallyApplied[N1] = new Array1PartiallyApplied[N1]
  def array2[N1 <: Nat, N2 <: Nat]                  = new Array2PartiallyApplied[N1, N2]

  def arrange[N <: Nat](
    n: N
  )(implicit toInt: ToInt[n.N], s: Shape1[N], vs: ValidShape[Shape1[N]]): NdArray[Int, Shape1[N]] =
    array1[N](Array.range(0, Nat.toInt(n)))

  def arrange[Start <: Nat, End <: Nat, Len <: Nat](start: Start, end: End)(implicit
    sToInt: ToInt[Start],
    eToInt: ToInt[End],
    diff: Diff.Aux[End, Start, Len],
    s: Shape1[Len],
    vs: ValidShape[Shape1[Len]]
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
    s: Shape1[Succ[Size]],
    vs: ValidShape[Shape1[Succ[Size]]]
  ): NdArray[Int, Shape1[Succ[Size]]] =
    array1(Array.range(Nat.toInt[Start], Nat.toInt[End], Nat.toInt[Interval]))
}
