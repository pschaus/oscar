package oscar.cp.constraints

import oscar.cp._
import oscar.cp.testUtils._

// Test the sum constraint on CPIntVar
class SumSuite extends TestSuite {

  test("sum0") {

    val s = CPSolver()

    val x0 = CPIntVar(0, 1)(s)
    val x1 = CPIntVar(0, 2)(s)
    val x2 = CPIntVar(0, 2)(s)
    val x3 = CPIntVar(0, 3)(s)

    val cumulatedCounters = Array(x0, x1, x2, x3)
    val n = cumulatedCounters.size
    val len = 2
    val min = 1
    val max = 2

    // 0-1 , 0-2 , 0-2 , 0-3

    for (i <- 0 to n - len) {
      var nbVal = cumulatedCounters(i + len - 1)
      if (i > 0) {
        nbVal = nbVal - cumulatedCounters(i - 1)
      }
      s.post(nbVal <= max)
      s.post(nbVal >= min)
      s.isFailed should be(false)
    }
  }

  test("sum1") {
    val cp = CPSolver()
    val x = Array(CPIntVar(0, 5)(cp), CPIntVar(1, 5)(cp), CPIntVar(0, 5)(cp))
    val y = CPIntVar(0, 100)(cp)
    cp.add(sum(x, y))
    y.min should be(1)
    y.max should be(15)
  }

  test("sum2") {
    val cp = CPSolver()
    val x = Array(CPIntVar(-5, 5)(cp), CPIntVar(1, 2)(cp), CPIntVar(0, 1)(cp))
    val y = CPIntVar(0, 100)(cp)
    cp.add(sum(x, y))
    x(0).min should be(-3)
    y.min should be(0)
    y.max should be(8)
  }

  test("sum3") {
    val cp = CPSolver()
    val x = Array(CPIntVar(0, 5)(cp), CPIntVar(0, 5)(cp), CPIntVar(0, 5)(cp))
    val y = CPIntVar(5)(cp)
    cp.add(sum(x, y))
    cp.add(x(1) === 0)
    cp.add(x(0) >= 1)
    x(0).max should be(5)

    x(2).min should be(0)
    x(2).max should be(4)
  }
  
  private val rand = new scala.util.Random(0)
  
  private def solve(solver: CPSolver, x: Array[CPIntVar], y: CPIntVar, decomp: Boolean = false): Int = {   
    if (decomp) solver.add(new Sum(x, y))
    else solver.add(sum(x, y))    
    solver.search { 
      val optVar = x.find(!_.isBound)
      if (optVar.isEmpty) noAlternative
      else {
        val variable = optVar.get
        val value = variable.min
        branch {
          solver.post(variable === value)
        }{
          solver.post(variable > value)
        }
      }   
    }
    val stats = solver.start()
    stats.nSols
  }

  test("sum4") {

    val cp = CPSolver()
    val x = Array(CPIntVar(-2, 5, "x0")(cp), CPIntVar(-4, 5, "x1")(cp), CPIntVar(3, 5, "x2")(cp))
    val y = CPIntVar(4, 5, "y")(cp)
    solve(cp, x, y, false) should be(solve(cp, x, y, true))
  }

  test("sum5") {
    val cp = CPSolver()
    val x = Array(CPIntVar(Set(-5, -3, 2, 8))(cp), CPIntVar(Set(-10, 8))(cp), CPIntVar(3, 5)(cp))
    val y = CPIntVar(3, 8)(cp)
    solve(cp, x, y, false) should be(solve(cp, x, y, true))
  }

  test("sum6") {
    val cp = CPSolver()
    val x = Array(CPIntVar(Set(-5, -3, 2, 8))(cp), CPIntVar(Set(-10, 8))(cp), CPIntVar(3, 5)(cp), CPIntVar(-10, -5)(cp))
    val y = CPIntVar(3, 8)(cp)
    solve(cp, x, y, false) should be(solve(cp, x, y, true))
  }

}