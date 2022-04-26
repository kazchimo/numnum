package ndarray

import ndarray.Shape.{Shape1, Shape2}
import org.scalatest.funspec.AnyFunSpec
import shapeless.Nat._
import shapeless._

class ShapeTest extends AnyFunSpec {
  describe("shape") {
    it("should return shape of ShapeHL") {
      assert(Shape.of1[_2].shape == _2 :: HNil)

      assert(Shape.of2[_2, _3].shape == _2 :: _3 :: HNil)
    }
  }

  describe("ndim") {
    it("should return ndim of ShapeHL") {
      assert(Shape.of1[_2].ndim == _1)

      assert(Shape.of2[_2, _3].ndim == _2)
    }
  }

  describe("headSize") {
    it("should return the number of head of S") {
      assert(new Shape1[_3].headSize == _3)
      assert(new Shape2[_3, _5].headSize == _3)
    }
  }

  describe("tailShape") {
    it("should return the tail of S") {
      assertDoesNotCompile("""new Shape1[_3].tailShape """)
      assert(new Shape2[_3, _5].tailShape == Shape.of1[_5])
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
