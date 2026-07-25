package oscar.algo.reversible

import oscar.cp.core.CPStore
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestSparseSet extends AnyFunSuite {

  test("test1") {
    val set = new SparseSet(0, 10, true)
    assert(set.getSize() == 0)
    for (v <- set.toArray) {
      assert(false)
    }
    set.insert(1)
    var valArray = set.getSortedVals
    assert(valArray.length == 1)
    assert(valArray(0) == 1)

    set.insert(2)
    set.insert(4)
    valArray = set.getSortedVals
    println(java.util.Arrays.toString(valArray))
    assert(valArray(0) == 1)
    assert(valArray(1) == 2)
    assert(valArray(2) == 4)
    assert(!set.hasValue(0))
    assert(!set.hasValue(3))
    assert(!set.hasValue(5))
  }

}
