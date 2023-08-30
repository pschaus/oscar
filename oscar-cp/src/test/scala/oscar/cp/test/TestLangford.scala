package oscar.cp.test


import oscar.cp._
import oscar.cp.testUtils._
import oscar.cp.core.CPPropagStrength

class TestLangford extends TestSuite {

  test("Test Langford count #solutions") {

    // return the number of solutions
    def test(k: Int, cons: CPPropagStrength) = {
      //
      // variables
      //
      implicit val cp = CPSolver()
      val position = Array.fill(2 * k)(CPIntVar(0 to 2 * k - 1))
      // channel positions to a solution array
      val solution = Array.fill(2 * k)(CPIntVar(1 to k))
      //
      // constraints
      //
      var numSols = 0
      val cons = Strong
      add(allDifferent(position), cons)
      for (i <- 1 to k) {
        add(position(i + k - 1) === (position(i - 1) + i + 1))

        add(elementVar(solution, position(i - 1), i), cons)
        add(elementVar(solution, position(k + i - 1), i), cons)
      }
      // symmetry breaking
      add(solution(0) < solution(2 * k - 1))
      search {
        binary(position, _.size, _.min)
      }

      val stat = start()
      stat.nSols

    }
    val ks = Array(4,6,8,10)
    val nSols = Array(1,0,150,0)
    //val nSols = Array()
    for ((k,i) <- ks.zipWithIndex) {
      for (cons <- CPPropagStrength.values()) {
        assert(test(k,cons) == nSols(i))
      }
    }
    
    
  }
}
