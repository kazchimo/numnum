package ndarray

trait Shape {
  type S <: Product
}

object Shape {
  trait Shape1[N1 <: DimN]             extends Shape { type S = Tuple1[N1] }
  trait Shape2[N1 <: DimN, N2 <: DimN] extends Shape { type S = (N1, N2)   }
}
