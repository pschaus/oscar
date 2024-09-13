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



class TestStretchWrapper extends TestSuite  {


  test("test stretch 1") {
    val cp = CPSolver()
    var x = Array.fill(6)(CPIntVar(0 to 2)(cp))
    val automaton = stretchAutomaton(x, 2, 2)
    cp.add(regular(x,automaton))
    cp.add(x(5) === 0)
    cp.isFailed should be(false)
    
    //  0-2  0-2  1-2  1-2  0   0
    //println(x.mkString(","))
    x(4).value should be(0)
    x(3).hasValue(0) should be(false)
    x(2).hasValue(0) should be(false)
      
  }
  
 
  

  

}
