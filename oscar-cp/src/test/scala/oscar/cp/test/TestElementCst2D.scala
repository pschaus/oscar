package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.constraints.ElementCst2D

/**
 * Test on Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestElementCst2D extends TestSuite {

  test("Test Element 1") {
    val cp = CPSolver()
    val T = Array(Array(1, 3, 2),
      Array(6, 8, 1))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(new ElementCst2D(T, x, y, z))

    x.min should be(0)
    x.max should be(1)
    y.min should be(0)
    y.max should be(2)
    z.min should be(1)
    z.max should be(8)
  }

  test("Test Element 2") {
    val cp = CPSolver()
    val T = Array(Array(1, 3, 2),
      Array(6, 8, 1))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(7 to 8)(cp)

    cp.add(new ElementCst2D(T, x, y, z))

    x.min should be(1)
    x.max should be(1)
    y.min should be(1)
    y.max should be(1)
    z.min should be(8)
    z.max should be(8)
  }

  test("Test Element 3") {
    val cp = CPSolver()
    val T = Array(Array(1, 7, 2),
      Array(6, 8, 1))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(7 to 8)(cp)

    cp.add(new ElementCst2D(T, x, y, z))

    x.min should be(0)
    x.max should be(1)
    y.min should be(1)
    y.max should be(1)
    z.min should be(7)
    z.max should be(8)
  }

  test("Test Element 4") {
    val cp = CPSolver()
    val T = Array(Array(1, 7, 2),
      Array(6, 8, 1))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(9 to 10)(cp)

    postAndCheckFailure(cp, new ElementCst2D(T, x, y, z))
    cp.isFailed should be(true)
  }

  test("Test Element 5") {
    val cp = CPSolver()
    val T = Array(Array(1, 3, 2),
      Array(6, 8, 1))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(0 to 10)(cp)

    cp.add(new ElementCst2D(T, x, y, z))
    cp.add(z >= 4)

    x.min should be(1)
    y.max should be(1)
    z.min should be(6)
    z.max should be(8)
  }

  test("Test Element 6") {
    val cp = CPSolver()
    val T = Array(Array(1, 3, 2),
      Array(1, 8, 4))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(0 to 10)(cp)

    cp.add(new ElementCst2D(T, x, y, z))
    cp.add(y !== 0)

    z.min should be(2)
    z.max should be(8)
  }

  test("Test Element 7") {
    val cp = CPSolver()
    val T = Array(Array(1, 3, 2),
      Array(1, 8, 4))
    val x = CPIntVar(-3 to 10)(cp)
    val y = CPIntVar(-4 to 14)(cp)
    val z = CPIntVar(0 to 10)(cp)

    cp.add(new ElementCst2D(T, x, y, z))
    cp.add(y !== 0)
    cp.add(z < 4)

    x.max should be(0)
    z.min should be(2)
    z.max should be(3)
  }

}
