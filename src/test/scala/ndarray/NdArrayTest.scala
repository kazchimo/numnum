package ndarray

import org.scalatest.funspec.AnyFunSpec
import shapeless.Nat._

class NdArrayTest extends AnyFunSpec {
  describe("size") {
    it("should return the number of elements in the array") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).size == 3)
    }
  }

  describe("ndim") {
    it("should return the number of dimensions in the array") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).ndim == 1)
      assert(NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).ndim == 2)
    }
  }

  describe("shape") {
    it("should return the shape of the array") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).shape == Tuple1(_3))
      assert(NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).shape == (_2, _3))
    }
  }

  describe("all") {
    it("should return true if all elements are true") {
      assert(NdArray.array1[_3](Array(true, true, true)).all)
      assert(NdArray.array2[_2, _3](Array(Array(true, true, true), Array(true, true, true))).all)
    }

    it("should return false if any element is false") {
      assert(!NdArray.array1[_3](Array(true, false, true)).all)
      assert(!NdArray.array2[_2, _3](Array(Array(true, false, true), Array(true, true, true))).all)
    }

    it("should return true if all elements are not zero") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).all)
      assert(NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).all)
    }

    it("should return false if any element is zero") {
      assert(!NdArray.array1[_3](Array(1, 0, 3)).all)
      assert(!NdArray.array2[_2, _3](Array(Array(1, 0, 3), Array(4, 5, 6))).all)
    }
  }

  describe("companion") {
    describe("array") {
      it("should create a new NdArray") {
        NdArray.array1[_3](Array(1, 2, 3))
      }

      it("should not compile without Shape type parameter") {
        assertDoesNotCompile("NdArray.array(Array(1, 2, 3))")
      }
    }
  }
}
