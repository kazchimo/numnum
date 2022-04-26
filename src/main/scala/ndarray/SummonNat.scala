package ndarray

import shapeless.Nat

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait SummonNat[N] {
  type Out

  val value: N

  def toInt: Int
}

object SummonNat {
  type Aux[N, O <: Nat] = SummonNat[N] { type Out = O }

  def apply[N <: Nat](implicit sn: SummonNat[N]): SummonNat[N] = sn

  implicit def summonNat[N <: Nat]: SummonNat[N] = macro NatMacros.materialize[N]
}

class NatMacros(val c: whitebox.Context) {
  import c.universe._

  def recursiveDealiase(tpe: Type): Type = tpe match {
    case TypeRef(_, sym, _) =>
      val dealiased = sym.asType.toType.dealias
      if (dealiased =:= sym.asType.toType) tpe
      else recursiveDealiase(dealiased)
    case _                  => tpe
  }

  def materialize[N: WeakTypeTag]: c.universe.Tree = {
    val tpe     = weakTypeOf[N]
    val natRepr = recursiveDealiase(tpe)

    q"""
      new SummonNat[$tpe] {
        val value: $natRepr = new $natRepr
        
        def toInt: Int = value.toInt
      }
      """
  }
}
