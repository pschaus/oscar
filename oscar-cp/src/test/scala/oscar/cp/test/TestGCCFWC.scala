/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.testUtils._
import oscar.cp.constraints.GCCFWC
import oscar.cp.core.CPPropagStrength

class TestGCCFWC extends TestSuite {

  test("Test 3: Variation of 2 that should have some solutions") {
    implicit val cp = new CPSolver()
    val x1 = CPIntVar(0, 2)
    val x2 = CPIntVar(1, 3)
    val x3 = CPIntVar(0, 3)
    val x = Array(x1, x2, x3)
    val minVal = 0
    val Low = Array(0, 0, 0)
    val Up = Array(2, 0, 0)
    add(new GCCFWC(x, minVal, Low, Up))
    search { binaryFirstFail(x) }
    start().nSols should be > 0
  }
  
  test("Test 4: verifying a solution") {
    implicit val cp = new CPSolver()
    val x1 = CPIntVar(0, 2)
    val x2 = CPIntVar(1, 3)
    val x3 = CPIntVar(0, 3)
    val x = Array(x1, x2, x3)
    val minVal = 0
    val Low = Array(0, 1, 0, 2)
    val Up = Array(3, 2, 3, 2)
    add(new GCCFWC(x, minVal, Low, Up))
    val sol = Array[Int](1, 3, 3)
    for (i <- 0 until x.length) {
      x(i).value should be(sol(i))
    }
  }

  test("Test 5: counting solutions") {
    implicit val cp = new CPSolver()
    val x1 = CPIntVar(0, 2)
    val x2 = CPIntVar(1, 3)
    val x3 = CPIntVar(0, 3)
    val x = Array(x1, x2, x3)
    val minVal = 0
    val Low = Array(0, 0, 0, 2)
    val Up = Array(0, 2, 3, 2)
    add(new GCCFWC(x, minVal, Low, Up))
    search { binaryFirstFail(x) }
    cp.start().nSols should be(2)
  }

  test("Test 6: counting solutions") {
    implicit val cp = new CPSolver()
    val x1 = CPIntVar(0, 2)
    val x2 = CPIntVar(1, 3)
    val x3 = CPIntVar(0, 3)
    val x = Array(x1, x2, x3)
    val minVal = 0
    val Low = Array(0, 0, 0, 0)
    val Up = Array(1, 1, 1, 1)
    add(new GCCFWC(x, minVal, Low, Up))
    search { binaryFirstFail(x) }
    cp.start().nSols should be(14)
  }

  test("Test 7: test mapping from AllDifferent to GCCFWC") {
    val theAllDiff = Array(
      Array(2,10,11),
      Array(11,14),
      Array(8,9,10,17),
      Array(8,9,10),
      Array(2,9,13),
      Array(8,10,17),
      Array(8,9,11),
      Array(11,13,17)
    )

    val removalOrder = Array(
      (4, Array(2)),
      (0, Array(11, 10)),
      (2, Array(8)),
      (3, Array(9, 10)),
      (5, Array(8)),
      (6, Array(8)),
      (2, Array(17, 10)),
      (4, Array(9)),
      (6, Array(9))
      //after this, there are no more calls to whenDomainChanges
    )

    val wrongSolution = Array(2,11,9,8,13,10,11,11)

    val solutionIsWrong = try {
      implicit val solver = CPSolver()
      val x = theAllDiff.map(domain => CPIntVar(domain))
      solver.add(oscar.cp.modeling.constraint.allDifferent(x), CPPropagStrength.Weak) //GCCFWC
      for ((idx, toRemove) <- removalOrder) {
        solver.add(toRemove.map(value => x(idx) !== value))
      }
      //once you have pushed removalOrder, the constraint was wrongly marked as Success
      for((value, idx) <- wrongSolution.zipWithIndex)
        solver.add(x(idx) === value)
      //this should have raised a NoSolutionException
      false
    }
    catch {
      case e: NoSolutionException => true
    }

    assert(solutionIsWrong)
  }
}

