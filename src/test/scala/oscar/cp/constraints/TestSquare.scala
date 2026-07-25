package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.cp.constraints.Square
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestSquare extends AnyFunSuite {

  test("testSquare1") {
    implicit val s: CPStore = new CPStore()
    val x = CPIntVar(-5, 5)
    val y = CPIntVar(-5, 16)
    s.post(new Square(x, y))
    assert(!s.isFailed)
    assert(x.min == -4)
    assert(x.max == 4)
    assert(y.max == 16)
    assert(y.min == 0)
  }

}
