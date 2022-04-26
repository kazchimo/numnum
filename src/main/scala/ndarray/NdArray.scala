package ndarray

import breeze.linalg.DenseMatrix
import ndarray.Shape.{Shape1, Shape2, applySummonNat}
import shapeless.ops.hlist.{LiftAll, Mapper}
import shapeless.{HList, Nat}

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

  def size[O <: Nat](implicit size: Size.Aux[s.ShapeHL, O]): O = s.size

  def reshape[ToShape <: Shape, Head <: Nat](shape: ToShape)(implicit
    sameSize: SameSize[S, ToShape],
    res: Reshape[ToShape],
    validShape: ValidShape[ToShape]
  ): NdArray[T, ToShape] = NdArray(values.reshape(res.row, res.column))(shape, validShape)
}

object NdArray {
  class Array1PartiallyApplied[N1 <: Nat] {
    def apply[T](value: Array[T])(implicit
      arrayConst: ArrayConstructor1.Aux[N1, T]
    ): NdArray[T, Shape1[N1]] = arrayConst(value)
  }

  class Array2PartiallyApplied[N1 <: Nat, N2 <: Nat] {
    def apply[T](value: Array[Array[T]])(implicit
      arrayConstructor2: ArrayConstructor2.Aux[N1, N2, T]
    ): NdArray[T, Shape2[N1, N2]] = arrayConstructor2(value)
  }

  def array1[N1 <: Nat]: Array1PartiallyApplied[N1] = new Array1PartiallyApplied[N1]
  def array2[N1 <: Nat, N2 <: Nat]                  = new Array2PartiallyApplied[N1, N2]

  def arrange[N <: Nat: Arrange1]: NdArray[Int, Shape1[N]] = Arrange1[N].apply

  def arrange[Start <: Nat, End <: Nat](implicit
    arrange: Arrange2[Start, End]
  ): NdArray[Int, Shape1[arrange.Out]] = arrange.apply

  def arrange[Start <: Nat, End <: Nat, Step <: Nat](implicit
    arrange: Arrange3[Start, End, Step]
  ): NdArray[Int, Shape1[arrange.Out]] = arrange.apply

  class FullPartiallyApplied[S <: Shape] {
    def apply[T](t: T)(implicit full: Full.Aux[S, T]): NdArray[T, S] = full(t)
  }

  def full[S <: Shape] = new FullPartiallyApplied[S]
}
