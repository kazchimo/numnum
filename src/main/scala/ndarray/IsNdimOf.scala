package ndarray

import shapeless.{=:!=, HList, Nat}

trait IsNdimOf[S, N] {
  type Out

  def apply: Out
}

object IsNdimOf {
  type Aux[S, N, Out0] = IsNdimOf[S, N] { type Out = Out0 }

  implicit def isNdimOf[S <: HList, N1 <: Nat, N2 <: Nat](implicit
    ndim: Ndim.Aux[S, N2],
    ev: N1 =:= N2
  ): Aux[S, N1, true] = new IsNdimOf[S, N1] {
    type Out = true

    def apply: Out = true
  }

  implicit def isNotNdimOf[S <: HList, N1 <: Nat, N2 <: Nat](implicit
    ndim: Ndim.Aux[S, N2],
    ev: N1 =:!= N2
  ): Aux[S, N1, false] = new IsNdimOf[S, N1] {
    type Out = false

    def apply: Out = false
  }
}
