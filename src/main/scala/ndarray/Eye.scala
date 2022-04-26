package ndarray

import breeze.linalg.DenseMatrix
import ndarray.NdArray.NdArray2
import shapeless.Nat
import shapeless.ops.nat.ToInt

trait Eye[N <: Nat] {
  def apply: NdArray2[Double, N, N]
}

object Eye {
  implicit def eye[N <: Nat: ToInt]: Eye[N] = new Eye[N] {
    def apply: NdArray2[Double, N, N] = NdArray(DenseMatrix.eye[Double](ToInt[N].apply))
  }
}
