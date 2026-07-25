package oscar.cp.util

import oscar.cp.testUtils._

import oscar.cp.util.NumberUtils
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestNumberUtils extends AnyFunSuite {

  private def isPerfectSquare(v: Int): Boolean = {
    val r = math.sqrt(v).toInt
    r * r == v
  }

  test("testPerfectSquare") {
    assert(NumberUtils.isPerfectSquare(8 * 8))
    assert(!NumberUtils.isPerfectSquare(8 * 9))
  }

  test("testCeilDiv") {
    assert(NumberUtils.ceilDiv(7, 2) == 4)
    assert(NumberUtils.ceilDiv(-7, -2) == 4)
    assert(NumberUtils.ceilDiv(-7, 2) == -3)
    assert(NumberUtils.ceilDiv(7, -2) == -3)
  }

  test("testFloorDiv") {
    assert(NumberUtils.floorDiv(385810, 100000) == 3)
    assert(NumberUtils.floorDiv(495700, 100000) == 4)
    assert(NumberUtils.floorDiv(-5, 2) == -3)
  }

}
