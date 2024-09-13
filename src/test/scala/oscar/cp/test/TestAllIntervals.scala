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

import oscar.cp._
import oscar.cp.testUtils._

class TestAllIntervals extends TestSuite {

  test("AllIntervals") {
    implicit val cp = CPSolver()
    
    val n = 6 
    val x = Array.fill(n)(CPIntVar(0 to n - 1))
    val diffs = Array.fill(n - 1)(CPIntVar(1 to n - 1))
    
    add(allDifferent(diffs), Strong)
    add(allDifferent(x), Strong)

    for (k <- 0 until n - 1) {
      add(diffs(k) === (x(k + 1) - (x(k))).abs)
    }

    // symmetry breaking
    add(x(0) < x(n - 1))
    add(diffs(0) < diffs(1))

    search { binaryStatic(x) } 
    
    val stats = start()
    assert(stats.nSols == 8)
  }
}
