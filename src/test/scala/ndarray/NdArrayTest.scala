package ndarray

import org.scalatest.funspec.AnyFunSpec
import shapeless.HNil
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
      assert(NdArray.array1[_3](Array(1, 2, 3)).shape == _3 :: HNil)
      assert(
        NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).shape == _2 :: _3 :: HNil
      )
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

    it("should return true on empty array") {
      assert(NdArray.array1[_0](Array[Int]()).all)
    }
  }

  describe("any") {
    it("should return true if any element is true") {
      assert(NdArray.array1[_3](Array(true, false, true)).any)
      assert(NdArray.array2[_2, _3](Array(Array(true, false, true), Array(true, true, true))).any)
    }

    it("should return false if all elements are false") {
      assert(!NdArray.array1[_3](Array(false, false, false)).any)
      assert(
        !NdArray.array2[_2, _3](Array(Array(false, false, false), Array(false, false, false))).any
      )
    }

    it("should return true if any element is not zero") {
      assert(NdArray.array1[_3](Array(1, 0, 3)).any)
      assert(NdArray.array2[_2, _3](Array(Array(1, 0, 3), Array(4, 5, 6))).any)
    }

    it("should return false if all elements are zero") {
      assert(!NdArray.array1[_3](Array(0, 0, 0)).any)
      assert(!NdArray.array2[_2, _3](Array(Array(0, 0, 0), Array(0, 0, 0))).any)
    }

    it("should return false on empty array") {
      assert(!NdArray.array1[_0](Array[Int]()).any)
    }
  }

  describe("arrange") {
    it("should return an array of the specified shape with the specified values") {
      val arr1 = NdArray.arrange(_3)
      assert(arr1.values.sameElements(Array(0, 1, 2)))
      assert(arr1.shape == _3 :: HNil)

      val arr2 = NdArray.arrange(_2, _5)
      assert(arr2.values.sameElements(Array(2, 3, 4)))
      assert(arr2.shape == _3 :: HNil)
      assertDoesNotCompile("NdArray.arrange(_5, _2)")

      val arr3 = NdArray.arrange(_3, _10, _2)
      assert(arr3.values.sameElements(Array(3, 5, 7, 9)))
      assert(arr3.shape == _4 :: HNil)
      assertDoesNotCompile("NdArray.arrange(_10, _3, _2)")
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
