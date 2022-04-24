package ndarray

import ndarray.Shape.{Shape1, Shape2}
import org.scalatest.funspec.AnyFunSpec

class NdArrayTest extends AnyFunSpec {
  describe("size") {
    it("should return the number of elements in the array") {
      assert(NdArray.array[Shape1[3]](Array(1, 2, 3)).size == 3)
    }
  }

  describe("ndim") {
    it("should return the number of dimensions in the array") {
      assert(NdArray.array[Shape1[3]](Array(1, 2, 3)).ndim == 1)
      assert(NdArray.array[Shape2[2, 3]](Array(Array(1, 2, 3), Array(4, 5, 6))).ndim == 2)
    }
  }

  describe("shape") {
    it("should return the shape of the array") {
      assert(NdArray.array[Shape1[3]](Array(1, 2, 3)).shape == Tuple1(3))
      assert(NdArray.array[Shape2[2, 3]](Array(Array(1, 2, 3), Array(4, 5, 6))).shape == (2, 3))
    }
  }

  describe("companion") {
    describe("array") {
      it("should create a new NdArray") {
        NdArray.array[Shape1[3]](Array(1, 2, 3))
      }

      it("should not compile without Shape type parameter") {
        assertDoesNotCompile("NdArray.array(Array(1, 2, 3))")
      }
    }
  }
}
