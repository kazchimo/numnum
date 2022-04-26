package ndarray

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.storage.Zero
import ndarray.Shape.{Shape1, Shape2, applySummonNat, sumNat}
import shapeless.Nat._1
import shapeless.ops.hlist.{LeftFolder, LiftAll, Mapper}
import shapeless.ops.nat.ToInt
import shapeless.{HList, Nat}

import scala.reflect.ClassTag

case class NdArray[T, S <: Shape](values: DenseMatrix[T])(implicit
  val s: S,
  validShape: ValidShape[S]
) {
  require(
    validShape(values),
    s"""should be valid shape
       |
       |value row: ${values.rows}
       |value col: ${values.cols}
       |""".stripMargin
  )

  def length: Int = values.size

  def toArray: Array[T] = values.toArray

  def ndim[Len <: Nat](implicit ndim: Ndim.Aux[s.ShapeHL, Len]): Len = s.ndim

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

  def arrange[N <: Nat: Arrange1](n: N): NdArray[Int, Shape1[N]] = Arrange1[N].apply(n)

  def arrange[Start <: Nat, End <: Nat, Len <: Nat](start: Start, end: End)(implicit
    arrange: Arrange2[Start, End, Len]
  ): NdArray[Int, Shape1[Len]] = arrange(start, end)

  def arrange[Start <: Nat, End <: Nat, Step <: Nat, Out <: Nat](
    start: Start,
    end: End,
    step: Step
  )(implicit arrange: Arrange3[Start, End, Step, Out]): NdArray[Int, Shape1[Out]] =
    arrange(start, end, step)

  class FullPartiallyApplied[S <: Shape] {
    def apply[T: Zero: ClassTag](t: T)(implicit full: Full[S]): NdArray[T, S] = full(t)
  }

  def full[S <: Shape] = new FullPartiallyApplied[S]
}
