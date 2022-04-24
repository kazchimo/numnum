package ndarray

trait Shape {
  type S <: Product

  def shape: S
}

object Shape {
  abstract class Shape1[N1 <: DimN: ValueOf] extends Shape {
    type S = Tuple1[N1]

    def shape: S = Tuple1(valueOf[N1])
  }

  abstract class Shape2[N1 <: DimN: ValueOf, N2 <: DimN: ValueOf] extends Shape {
    type S = (N1, N2)

    def shape: S = (valueOf[N1], valueOf[N2])
  }

  implicit def shape1[N1 <: DimN: ValueOf]: Shape1[N1]                          = new Shape1[N1] {}
  implicit def shape2[N1 <: DimN: ValueOf, N2 <: DimN: ValueOf]: Shape2[N1, N2] =
    new Shape2[N1, N2] {}
}
