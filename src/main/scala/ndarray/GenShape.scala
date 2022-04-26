package ndarray

import shapeless.{HList, Nat, Poly1}
import shapeless.ops.hlist.{LiftAll, Mapper}

trait GenShape[S] {
  type Out

  def apply: Out
}

object GenShape {
  type Aux[S, O0] = GenShape[S] { type Out = O0 }

  implicit def genShape[S <: HList, Instances <: HList](implicit
    liftAll: LiftAll.Aux[SummonNat, S, Instances],
    mapper: Mapper.Aux[applySummonNat.type, Instances, S]
  ): Aux[S, S] = new GenShape[S] {
    type Out = S

    override def apply: Out = liftAll.instances.map(applySummonNat)
  }

  object applySummonNat extends Poly1 {
    implicit def caseHasSummonNat[N <: Nat]: Case.Aux[SummonNat[N], N] = at(_.value)
  }
}
