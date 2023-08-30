/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.test

import oscar.cp._
import oscar.cp.testUtils._

class TestMagicSquare extends TestSuite {

  test("MagicSquare") {
    implicit val cp = CPSolver()
    val n = 3
    val x = Array.fill(n, n)(CPIntVar(1 to n * n))
    val s = (n * (n * n + 1)) / 2;
    val diag1 = Array.tabulate(n)(i => x(i)(i))
    val diag2 = Array.tabulate(n)(i => x(i)(n - i - 1))
    var nbSol = 0
    add(allDifferent(x.flatten), Weak)
    add(sum(diag1) === s)
    add(sum(diag2) === s)
    for (i <- 0 until n) {
      add(sum(0 until n)(j => x(i)(j)) === s)
      add(sum(0 until n)(j => x(j)(i)) === s)
    }
    search {
      binaryFirstFail(x.flatten.toSeq)
    }
    start().nSols should be(8)
  }
}
