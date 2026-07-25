package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.cp._
import oscar.cp.constraints.BinaryKnapsack
import oscar.cp.core.CPPropagStrength
import oscar.cp.testUtils.TestSuite
import oscar.cp.util.ArrayUtils

class TestBinaryKnapsack extends TestSuite {

  test("testa") {
    val s = CPSolver()
    val b = Array.tabulate(111)(_ => CPBoolVar()(s))
    val l = CPIntVar(12 to 44)(s)
    val w = Array(
      2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7,
      8, 8, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 12, 12, 13, 13, 13, 13, 13, 14, 14,
      15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 18, 18, 19, 19, 19, 19, 19, 20, 20,
      20, 20, 20, 20, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 25, 26, 26,
      26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 30, 30, 30, 30
    )

    val bkp = new BinaryKnapsack(b, w, l)
    s.add(bkp, CPPropagStrength.Weak)
    s.add(bkp, CPPropagStrength.Strong)

    s.add(new oscar.cp.constraints.DiffVal(b(0), 0))
    s.isFailed should be(false)
  }

  test("testb") {
    val cp = CPSolver()
    val n = 20
    val x = Array.tabulate(n)(_ => CPBoolVar()(cp))
    val values = new Array[Int](n)
    val values2 = new Array[Int](n)
    for (i <- 0 until n) {
      values(i) = i + 1
      values2(i) = values(i) * values(i)
    }
    cp.add(new BinaryKnapsack(x, values, ArrayUtils.sum(values) / 2), CPPropagStrength.Weak)
    cp.add(new BinaryKnapsack(x, values2, ArrayUtils.sum(values2) / 2), CPPropagStrength.Weak)
    cp.add(new BinaryKnapsack(x, values, ArrayUtils.sum(values) / 2), CPPropagStrength.Strong)
    cp.add(new BinaryKnapsack(x, values2, ArrayUtils.sum(values2) / 2), CPPropagStrength.Strong)

    val sol = Array(true, false, false, true, false, false, true, true, true, true, false, true, false, false, false, true, false, true, false, true)
    for (i <- sol.indices) {
      if (i == sol.length / 2) {
        cp.add(new BinaryKnapsack(x, values, ArrayUtils.sum(values) / 2), CPPropagStrength.Weak)
        cp.add(new BinaryKnapsack(x, values2, ArrayUtils.sum(values2) / 2), CPPropagStrength.Weak)
        cp.add(new BinaryKnapsack(x, values, ArrayUtils.sum(values) / 2), CPPropagStrength.Strong)
        cp.add(new BinaryKnapsack(x, values2, ArrayUtils.sum(values2) / 2), CPPropagStrength.Strong)
      }

      if (sol(i)) {
        cp.add(x(i).constraintTrue)
      } else {
        cp.add(x(i).constraintFalse)
      }
    }
    cp.add(new BinaryKnapsack(x, values, ArrayUtils.sum(values) / 2), CPPropagStrength.Weak)
    cp.add(new BinaryKnapsack(x, values2, ArrayUtils.sum(values2) / 2), CPPropagStrength.Weak)
    cp.add(new BinaryKnapsack(x, values, ArrayUtils.sum(values) / 2), CPPropagStrength.Strong)
    cp.add(new BinaryKnapsack(x, values2, ArrayUtils.sum(values2) / 2), CPPropagStrength.Strong)

    cp.isFailed should be(false)
  }

  test("testc") {
    val cp = CPSolver()
    val x = Array.tabulate(3)(_ => CPBoolVar()(cp))
    val values = Array(43, 23, 23)
    val c = CPIntVar(1 to 82)(cp)

    cp.add(new BinaryKnapsack(x, values, c), CPPropagStrength.Strong)

    cp.isFailed should be(false)
    c.getMin should be(23)
    c.getMax should be(66)
  }
}
