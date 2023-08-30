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

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._

class StableMarriage extends TestSuite {

  test("StableMarriage") {
    val n = 5

    val Women = 0 until n
    val Men = 0 until n

    // for each man, what is his ranking for the women (higher is better)
    val rankWomen = Array(Array(1, 2, 4, 3, 0),
      Array(1, 3, 2, 0, 4),
      Array(4, 2, 1, 3, 0),
      Array(0, 4, 3, 2, 1),
      Array(3, 2, 1, 0, 4))

    // for each woman, what is her ranking for the men (higher is better)			
    val rankMen = Array(Array(0, 1, 3, 2, 4),
      Array(2, 3, 0, 4, 1),
      Array(3, 2, 4, 1, 0),
      Array(0, 4, 1, 3, 2),
      Array(4, 1, 2, 0, 3))

    val cp = CPSolver()

    val wife = Array.fill(n)(CPIntVar(Women)(cp)) // wife(i) is the woman chosen for man i
    val husband = Array.fill(n)(CPIntVar(Men)(cp)) // husband(j) is the man chosen for woman j

    for (m <- Men) {
      cp.add(elementVar(husband, wife(m), m))
    }
    for (w <- Women) {
      cp.add(elementVar(wife, husband(w), w))
    }

    for (m <- Men; w <- Women) {
      val pref_m = element(rankMen(m), wife(m), Weak) // preference of m for his wife
      val pref_w = element(rankWomen(w), husband(w), Weak) // preference of w for her husband

      cp.add((pref_m ?> rankMen(m)(w)) ==> (pref_w ?< rankWomen(w)(m)))
      cp.add((pref_w ?> rankWomen(w)(m)) ==> (pref_m ?< rankMen(m)(w)))
    }

    cp.search {
      binaryStatic(wife)
    } onSolution {
      wife.map(_.value) should be(Array(0, 2, 1, 4, 3))
      husband.map(_.value) should be(Array(0, 2, 1, 4, 3))
    }
    cp.start().nSols should be(1)

  }

}
