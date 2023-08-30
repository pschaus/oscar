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


class TestSequence2 extends TestSuite  {
   
  test("test1") { 
	  val cp = CPSolver()
	  val x = Array.fill(5)(CPIntVar(Set(1,4,7))(cp))	  
		cp.add(new SequenceDecomposition(x,Set(1,4),l=3,min=2,max=2))
	  cp.search {
	    binaryFirstFail(x)
	  }
	  cp.start().nSols should be(32)
  }
  
  test("test2") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPIntVar(0 to 1)(cp))
	  cp.add(new SequenceDecomposition(x,Set(1),l=2,min=1,max=2))
	  cp.add(x(2) === 0)
	  x(3).isBound should be(true)
	  x(1).isBound should be(true)
	  x(1).value should be(1)
	  x(3).value should be(1)
	  cp.isFailed should be(false)
  } 
  
  test("test3") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPIntVar(1 to 5)(cp))
	  cp.add(new SequenceDecomposition(x,Set(3),l=2,min=1,max=2))
	  cp.add(x(2) === 2)
	  x(3).isBound should be(true)
	  x(1).isBound should be(true)
	  x(1).value should be(3)
	  x(3).value should be(3)
	  cp.isFailed should be(false)
  }
  
  test("test4") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPIntVar(1 to 5)(cp))
	  cp.add(new SequenceDecomposition(x,Set(2,3),l=2,min=1,max=2))
	  cp.add(x(2) === 1)
	  x(1).toSet should be(Set(2,3))
	  x(3).toSet should be(Set(2,3))
	  cp.isFailed should be(false)
  }
  
  test("test5") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPIntVar(1 to 5)(cp))
	  cp.add(new SequenceDecomposition(x,Set(4),l=1,min=1,max=1))
	  x.forall(_.isBoundTo(4)) should be(true)
   }
}


