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


class TestMinimum extends TestSuite  {


  test("Minimum1") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    
    var y = CPIntVar(0 to 6)(cp)
   
    cp.add(minimum(x) === y)
    
    y.min should be(1)
    y.max should be(3)
    
    x.foreach(w => cp.add(w >= 2))
    
    y.min should be(2)


  }  
  
  test("Minimum2") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    
    var y = CPIntVar(0 to 6)(cp)
   
    cp.add(minimum(x) === y)
    
    cp.add(y >= 2)
    
    for (w <- x) {
      w.min should be(2)
    }
  } 
  
  test("Minimum3") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    
    var y = CPIntVar(0 to 6)(cp)
   
    cp.add(minimum(x) === y)
    
    cp.add(y >= 2)
    
    cp.add(x(0) >= 3)
    cp.add(x(1) >= 3)
    
    y.min should be (2)
    
    
    cp.add(x(2) >= 3)
    
    y.value should be (3)
  }   

}
