package ndarray

import breeze.linalg.DenseMatrix
import ndarray.Shape.{Shape1, Shape2}
import org.scalatest.funspec.AnyFunSpec
import shapeless.HNil
import shapeless.Nat._

class NdArrayTest extends AnyFunSpec {
  describe("length") {
    it("should return the number of elements in the array") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).length == 3)
    }
  }

  describe("size") {
    it("should return the number of elements in the array") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).size.toInt == 3)
      assert(NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).size.toInt == 6)
    }
  }

  describe("ndim") {
    it("should return the number of dimensions in the array") {
      assert(NdArray.array1[_3](Array(1, 2, 3)).ndim == _1)
      assert(NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).ndim == _2)
    }

    it("should not be defined for ambiguous arrays") {
      assertDoesNotCompile("NdArray.ambiguous(Array(1, 2, 3)).ndim")
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

  describe("reshape") {
    it("should return a new array with the specified shape") {
      assert(
        NdArray.arrange[_10].reshape[Shape2[_2, _5]].values == DenseMatrix
          .create(2, 5, Array.range(0, 10))
      )
      assert(
        NdArray.arrange[_10].reshape[Shape2[_5, _2]].values == DenseMatrix
          .create(5, 2, Array.range(0, 10))
      )
      assert(
        NdArray.arrange[_10].reshape[Shape2[_2, _5]].reshape[Shape1[_10]].values == DenseMatrix
          .create(1, 10, Array.range(0, 10))
      )
    }

    it("should not compile if the shape is not compatible") {
      assertDoesNotCompile("""
          NdArray.arrange[_10].reshape[Shape2[_2, _2]]
        """)

      assertDoesNotCompile("""
          NdArray.arrange[_10].reshape[Shape2[_2, _5]].reshape[Shape1[_9]]
        """)
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
      val arr1 = NdArray.arrange[_3]
      assert(arr1.toArray.sameElements(Array(0, 1, 2)))
      assert(arr1.shape == _3 :: HNil)

      val arr2 = NdArray.arrange[_2, _5]
      assert(arr2.toArray.sameElements(Array(2, 3, 4)))
      assert(arr2.shape == _3 :: HNil)
      assertDoesNotCompile("NdArray.arrange(_5, _2)")

      val arr3 = NdArray.arrange[_3, _10, _2]
      assert(arr3.toArray.sameElements(Array(3, 5, 7, 9)))
      assert(arr3.shape == _4 :: HNil)
      assertDoesNotCompile("NdArray.arrange(_10, _3, _2)")
    }
  }

  describe("t") {
    it("should return the transpose of the array") {
      assert(
        NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).t.values == DenseMatrix
          .create(3, 2, Array(1, 2, 3, 4, 5, 6))
      )
    }
  }

  describe("companion") {
    describe("constructor") {
      it("should throw if invalid shape") {
        assertThrows[IllegalArgumentException](
          NdArray[Int, Shape1[_2]](DenseMatrix.create(2, 1, Array(1, 2)))
        )
        assertThrows[IllegalArgumentException](NdArray.array1[_3](Array(1, 2)))
        assertThrows[IllegalArgumentException](
          NdArray.array2[_2, _3](Array(Array(1, 2), Array(3, 4)))
        )
      }
    }

    describe("array") {
      it("should create a new NdArray") {
        val arr1 = NdArray.array1[_3](Array(1, 2, 3)).values
        assert(arr1 == DenseMatrix.create(1, 3, Array(1, 2, 3)))
        assert(arr1.cols == 3 && arr1.rows == 1)

        val arr2 = NdArray.array2[_2, _3](Array(Array(1, 2, 3), Array(4, 5, 6))).values
        println(arr2)
        assert(arr2 == DenseMatrix.create(2, 3, Array(1, 4, 2, 5, 3, 6)))
        assert(arr2.cols == 3 && arr2.rows == 2)
      }

      it("should not compile without Shape type parameter") {
        assertDoesNotCompile("NdArray.array(Array(1, 2, 3))")
      }
    }

    describe("full") {
      it("should create a new NdArray filled with given value") {
        assert(NdArray.full[Shape1[_3]](1).values == DenseMatrix.create(1, 3, Array(1, 1, 1)))
        assert(
          NdArray.full[Shape2[_2, _3]](1).values == DenseMatrix
            .create(2, 3, Array(1, 1, 1, 1, 1, 1))
        )
      }
    }

    describe("ones") {
      it("should create a new NdArray filled with ones") {
        assert(NdArray.ones[Shape1[_3]].values == DenseMatrix.create(1, 3, Array(1, 1, 1)))
        assert(
          NdArray.ones[Shape2[_2, _3]].values == DenseMatrix.create(2, 3, Array(1, 1, 1, 1, 1, 1))
        )
      }
    }

    describe("zeros") {
      it("should create a new NdArray filled with zeros") {
        assert(NdArray.zeros[Shape1[_3]].values == DenseMatrix.create(1, 3, Array(0, 0, 0)))
        assert(
          NdArray.zeros[Shape2[_2, _3]].values == DenseMatrix.create(2, 3, Array(0, 0, 0, 0, 0, 0))
        )
      }
    }
  }
}
