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



class TestConstraintDegree extends TestSuite  {


  test("ConstraintDegree1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    
    cp.post(x(0) !== x(1))
    cp.post(x(0) !== x(2))
    x(0).constraintDegree should be(2)
    x(1).constraintDegree should be(1)
    	
  }
  


}
