package oscar.cp.test


import oscar.cp._
import oscar.cp.testUtils._

class TestGolomb extends TestSuite {

  test("Golomb Ruler") {
    
    // return the best ruler with n ticks
    def test(n: Int) = {
      implicit val cp = CPSolver()
      val marks = Array.fill(n)(CPIntVar(0 to n * n))
      val obj = marks(n - 1)
      var best = Int.MaxValue
      cp.silent = true
      minimize(obj)
      // we break symmetries to put the marks increasing
      add(marks(0) === 0)
      for (i <- 0 until n - 1) {
        add(marks(i) < marks(i + 1))
      }
      add(allDifferent(for (i <- 0 until n; j <- i + 1 until n) yield marks(j) - marks(i)), Strong)
      // break the symmetries between differences
      add(marks(1) - marks(0) < marks(n - 1) - marks(n - 2))
      search { binaryFirstFail(marks) }
      onSolution { best = obj.value }

      println("Reached")
      start()
      best
    }

    test(6) should be(17)
    test(7) should be(25)
  }
}
