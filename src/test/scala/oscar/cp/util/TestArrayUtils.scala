package oscar.cp.util

import oscar.cp.testUtils._

import oscar.cp.util.ArrayUtils
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestArrayUtils extends AnyFunSuite {

  test("testSort") {
    val vals1: Array[Integer] = Array(1, 3, 6, 1, 3)
    val vals2: Array[Int] = Array(1, 3, 6, 1, 3)
    ArrayUtils.sort(vals1, vals2)
    for (i <- 0 until vals2.length - 1) {
      assert(vals1(i) <= vals1(i + 1))
    }
  }

}
