package ndarray

import ndarray.Shape.{Shape1, Shape2}

trait NDim[S <: Shape] {
  val ndim: Int
}

object NDim {
  implicit def ndim1[N1 <: DimN]: NDim[Shape1[N1]] = new NDim[Shape1[N1]] {
    val ndim: Int = 1
  }

  implicit def ndim2[N1 <: DimN, N2 <: DimN]: NDim[Shape2[N1, N2]] = new NDim[Shape2[N1, N2]] {
    val ndim: Int = 2
  }

}
