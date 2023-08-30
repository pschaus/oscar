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


class TestAnd extends TestSuite  {
  
  
  test("and1") { 
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPBoolVar()(cp))
	  val B = CPBoolVar()(cp)
	  cp.add(new And(A,B))
	  cp.add(A(0) === 0)
	  B.isBoundTo(0) should be(true)
  }  

  test("and2") { 
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPBoolVar()(cp))
	  val B = CPBoolVar()(cp)
	  cp.add(new And(A,B))
	  cp.add(A(0) === 1)
	  cp.add(A(1) === 1)
	  B.isBoundTo(1) should be(false)
	  cp.add(A(2) === 1)
	  B.isBoundTo(1) should be(true)
  }
  
  test("and3") { 
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPBoolVar()(cp))
	  val B = CPBoolVar()(cp)
	  cp.add(new And(A,B))
	  cp.add(B === 0)
	  cp.add(A(0) === 1)
	  A(1).isBound should be(false)
	  cp.add(A(1) === 1)
	  A(2).isBoundTo(0) should be(true)
  } 
  
  test("and4") { 
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPBoolVar()(cp))
	  val B = CPBoolVar()(cp)
	  cp.add(new And(A,B))
	  cp.add(B === 1)
	  A.forall(_.isBoundTo(1)) should be(true)
  }    


}
