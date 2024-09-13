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


class TestOperator extends TestSuite  {
  
  
  test("unary operator") { 
	  val cp = CPSolver()
	  val A = CPIntVar(8)(cp)
	  val B = CPIntVar(-10 to 10)(cp)
	  val C = -A
	  C.value should be(-8)
	  cp.post(B === -C)
	  B.value should be(8)
  }  
  
  test("boolean unary operator 1") { 
	  val cp = CPSolver()
	  val A = CPBoolVar(true)(cp)
	  val C = -A
	  C.value should be(-1)
  }  

  test("boolean unary operator 2") { 
	  val cp = CPSolver()
	  val A = CPBoolVar(true)(cp)
	  val C = !A
	  C.value should be(0)
  }  
  
  test("boolean unary operator 3") { 
	  val cp = CPSolver()
	  val A = CPBoolVar(true)(cp)
	  val C = !(!A)
	  C.isTrue should be(true)
  }
  
  test("boolean unary operator 4") { 
	  val cp = CPSolver()
	  val A = CPBoolVar()(cp)
	  val B = CPBoolVar()(cp)
	  cp.add((!A || B) === (A ==> B))
	  cp.search {
	    binaryStatic(Seq(A,B))
	  }
	  cp.start().nSols should be(4)
  }
  
  test("boolean unary operator 5") { 
	  val cp = CPSolver()
	  val A = CPBoolVar()(cp)
	  val B = CPBoolVar()(cp)
	  cp.add(A && B)
	  A.isTrue should be(true)
	  B.isTrue should be(true)
  }   

 


}
