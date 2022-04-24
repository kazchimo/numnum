package ndarray

import ndarray.Shape.{Shape1, Shape2}
import org.scalatest.funspec.AnyFunSpec
import shapeless.Nat._

class ShapeTest extends AnyFunSpec {
  describe("headSize") {
    it("should return the number of head of S") {
      assert(new Shape1[_3].headSize == 3)
      assert(new Shape2[_3, _5].headSize == 3)
    }
  }
}
