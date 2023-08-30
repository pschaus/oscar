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

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._
import collection.immutable.SortedSet

class TestDudeney extends TestSuite  {


  test("Dudeney") {
    
    val sol = Set(1,512,4913,5832,17576,19683)
      
    val n = 5

    val cp = new CPSolver()
    var nbSol = 0

    val x = (0 until n).map(v => CPIntVar(0 to 9)(cp))
    val nb = CPIntVar(1 to math.pow(10, n).toInt - 1)(cp)
    val s = CPIntVar(1 to 9 * n)(cp)

    cp.add(nb === (s * s * s))
    cp.add(sum(0 until n)(i => x(i) * (math.pow(10, (n - i - 1)).toInt)) === nb)
    cp.add(sum(x) === s)
    cp.search {
      binaryFirstFail(x)
    } onSolution {
      sol.contains(nb.value) should be(true)
      nbSol += 1
    }
    cp.start().nSols should be(sol.size)

    
  }  
  

  


}
