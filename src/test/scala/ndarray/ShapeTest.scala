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

  describe("isShape1") {
    it("should return true if S is Shape1") {
      assert(new Shape1[_3].isShapeOf(_1))
      assert(new Shape2[_3, _5].isShapeOf(_2))
    }
  }

  describe("size") {
    it("should return the number of elements of S") {
      assert(new Shape1[_3].size.toInt == 3)
      assert(new Shape2[_3, _5].size.toInt == 15)
    }
  }
}