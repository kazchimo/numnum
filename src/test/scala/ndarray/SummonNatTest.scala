package ndarray

import org.scalatest.funspec.AnyFunSpec
import shapeless.Nat._

class SummonNatTest extends AnyFunSpec {
  describe("Summoning") {
    it("should summon for Nat") {
      assert(SummonNat[_0].value.isInstanceOf[_0])
      assert(SummonNat[_22].value.isInstanceOf[_22])
    }

    it("should not summon for other type") {
      assertDoesNotCompile("SummonNat[String]")
    }
  }

  describe("toInt") {
    it("should convert Nat to Int") {
      assert(SummonNat[_0].toInt == 0)
      assert(SummonNat[_22].toInt == 22)
    }
  }
}
