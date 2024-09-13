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


class TestMultiplication extends TestSuite  {


  test("Multiplication 1") {
    val cp = CPSolver()

    val x = CPIntVar(-10 to 10)(cp)
    val y = CPIntVar(Set(-70,-50,50,70))(cp)
    val z = CPIntVar(100 to 100)(cp)
    	
    cp.add(new oscar.cp.constraints.MulVar(x,y,z)); // should post a MulCteRes because z is fixed
    	
    var nbSol = 0
    cp.search {
      binaryFirstFail(Array(x,y))
    } onSolution {
      ((x.isBoundTo(-2) && y.isBoundTo(-50)) || (x.isBoundTo(2) && y.isBoundTo(50))) should be(true)
    }
    cp.start().nSols should be(2)

  }
  
  test("Multiplication 2") {
    val cp = CPSolver()

    val x = CPIntVar(-1 to 0)(cp)
    val y = CPIntVar(0 to 1)(cp)
    cp.post(x*x === y)
    	
    cp.isFailed should be(false)

  }
  
  test("Multiplication 3") {
    val cp = CPSolver()

    val x = CPIntVar(-10 to -1)(cp)
    val y = CPIntVar(3 to 9)(cp)
    cp.add(x*x === y)
    	
    cp.isFailed should be(false)

    x.min should be (-3)
    x.max should be (-2)
    
  }  
  
  test("Multiplication 4: Guess the number") {
    implicit val cp = CPSolver()
    val digits = Array.fill(5)(CPIntVar(0 to 9)(cp))   
    // with a one after (larger one)
    val nb1 =  digits(0)*100000 + digits(1)*10000 + digits(2)*1000 +  digits(3)*100 + digits(4)*10 + 1
    // with a one before (smaller one)
    val nb2 =  CPIntVar(100000)(cp) + digits(0)*10000 + digits(1)*1000 +  digits(2)*100 + digits(3)*10 + digits(4)
    var nbsol = 0
    cp.add(nb1 === (nb2*3))
    search {
      binaryStatic(digits)
    } 
    onSolution {
      nb1.value should be(428571)
      nb2.value should be(142857)      
    }
    cp.start().nSols should be(1)  
  }    
}
