package ndarray

import ndarray.Shape.Shape1
import org.scalatest.funspec.AnyFunSpec

class NdArrayTest extends AnyFunSpec {
  describe("size") {
    it("should return the number of elements in the array") {
      assert(NdArray.array[Shape1[3]](Array(1, 2, 3)).size == 3)
    }
  }

  describe("companion") {
    describe("array") {
      it("should create a new NdArray") {
        NdArray.array[Shape1[3]](Array(1, 2, 3))
      }
    }
  }
}
