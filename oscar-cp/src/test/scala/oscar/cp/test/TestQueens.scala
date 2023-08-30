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
import oscar.cp.core.CPPropagStrength

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestQueens extends TestSuite {

  test("Queens") {

    def nbSol(n: Int, l: CPPropagStrength) = {
      implicit val cp = CPSolver()
      val Queens = 0 until n
      //variables
      val queens = for (i <- Queens) yield CPIntVar(1 to n)
      var nbsol = 0
      add(allDifferent(queens), l)
      add(allDifferent(for (i <- Queens) yield queens(i) + i), l)
      add(allDifferent(for (i <- Queens) yield queens(i) - i), l)
      search {
        queens.find(!_.isBound) match {
          case None    => noAlternative
          case Some(x) => branchAll(1 to n)(v => add(x === v))
        }
      }
      val stat = cp.start()
      stat.nSols
    }
    nbSol(7, Weak) should be(40)
    nbSol(7, Medium) should be(40)
    nbSol(7, Strong) should be(40)
  }
}
