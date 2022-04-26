package ndarray

import breeze.linalg.DenseVector
import ndarray.Shape.Shape1
import shapeless.Nat
import shapeless.ops.nat.ToInt

trait Linspace[Start, End, Count <: Nat] {
  def apply: NdArray[Double, Shape1[Count]]
}

object Linspace {
  implicit def linspace[Start <: Nat: ToInt, End <: Nat: ToInt, Count <: Nat: ToInt]
    : Linspace[Start, End, Count] = {
    val start = ToInt[Start].apply()
    val end   = ToInt[End].apply()
    val count = ToInt[Count].apply()

    val space = (end - start).toDouble / (count.toDouble - 1)

    new Linspace[Start, End, Count] {
      def apply: NdArray[Double, Shape1[Count]] = {
        val data = DenseVector.tabulate[Double](count)(i => start + i * space)
        NdArray(data.asDenseMatrix)
      }
    }
  }
}
