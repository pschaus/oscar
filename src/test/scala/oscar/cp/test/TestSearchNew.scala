package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite


import oscar.cp._

class TestSearchNew extends TestSuite {

  test("ids search, bug #36") {
    implicit val cp = new CPSolver()
    var nbSol = 0
    cp.onSolution { nbSol += 1 }
    val x = Array(CPIntVar(0)(cp))
    cp.silent = true
    cp.minimize(x(0))
    search { binaryFirstFail(x) }
    val stat = cp.start()
    stat.nSols should be(1)
    nbSol should be(1)
  }

  test("explocompleted") {
    val cp = CPSolver()

    var nbSol = 0
    cp.onSolution { nbSol += 1 }

    val x = Array.tabulate(3)(i => CPBoolVar()(cp))

    cp.search {
      binaryStatic(x)
    }

    cp.start(nSols = 3).completed should be(false)
    cp.start().completed should be(true)
    cp.start(nSols = 3).completed should be(false)
    cp.start().completed should be(true)
    cp.start(failureLimit = 3).completed should be(false)
  }

  test("timelimit") {
    implicit val cp = CPSolver()
    val x = Array.fill(40)(CPIntVar(0 to 1)(cp))

    cp.silent = true

    var t0 = System.currentTimeMillis()
    search {
      binaryStatic(x)
    }

    val stat = cp.start(timeLimit = 2)
    val time: Int = ((System.currentTimeMillis() - t0) / 1000).toInt
    time should be >= (2)
    stat.completed should be(false)
    time should be <= (4)
    stat.completed should be(false)
  }

  test("optimize") {

    val cp = new CPSolver()
    val x = CPIntVar(Array(1, 5, 9, 10))(cp)
    cp.silent = true
    cp.minimize(x)
    var best = 0
    cp.onSolution { best = x.value }
    cp.search {
      binaryStaticIdx(Array(x), i => x.max)
    }
    val stat = cp.start()
    stat.nSols should be(4)
    best should be(1)

  }

  test("test 2 dfs") {

    val cp = CPSolver()
    val x = Array.fill(2)(CPIntVar(1 to 2)(cp))
    val y = Array.fill(2)(CPIntVar(1 to 2)(cp))

    //def dom(x: CPIntVar) = (x.min to x.max).filter(x.hasValue(_))

    var nbSol = 0
    cp.onSolution { nbSol += 1 }

    cp.search {
      binaryFirstFail(x) ++ binaryFirstFail(y)
    }

    val stat = cp.start()
    nbSol should equal(16)
    stat.nSols should be(16)
  }

  test("test 3 split") {

    val cp = CPSolver()
    val x = Array.fill(4)(CPIntVar(1 to 2)(cp))

    //def dom(x: CPIntVar) = (x.min to x.max).filter(x.hasValue(_))

    var nbSol = 0
    cp.onSolution { nbSol += 1 }

    cp.search {
      binaryFirstFail(x, _.min)
    }
    val nFails = cp.start().nFails
    cp.search {
      binarySplit(x, _.size)
    }
    cp.start().nFails should be(nFails)
  }

  test("test number of push") {
    val cp = CPSolver()
    val x = Array.fill(4)(CPIntVar(0 to 1)(cp))
    cp.pushState()
    x.foreach(y => cp.add(y === 0))
    cp.search {
      binaryFirstFail(x, _.min)
    }
    cp.start()
    cp.start(nSols = 1)
    cp.pop()
    x.foreach(_.size == 2)
  }

}
