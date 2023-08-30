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

/**
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestSetDisjoint extends TestSuite  {


  test("Test SetDisjoint1") {
    var nbSol = 0
    val cp = CPSolver()
    var x = CPSetVar(Set(1, 2, 3, 4), Set(1, 2))(cp)
    var y = CPSetVar(Set(1, 2, 3, 4, 5), Set(5))(cp)
    cp.post(disjoint(x,y))
    y.isPossible(1) should be(false)
    y.isPossible(2) should be(false)
    cp.post(x ++ 3)
    y.isPossible(3) should be(false)
    
  }
  

  
}
  
