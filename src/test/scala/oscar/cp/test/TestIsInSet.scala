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


class TestInSet extends TestSuite  {
  
  
  test("test1") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(1,4,7))(cp)
	  val b = x.isIn(Set(1,4,7))
	  b.isTrue should be(true)
  }
  
  test("test2") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,8))(cp)
	  val b = x.isIn(Set(1,4,7))
	  b.isTrue should be(false)
  }
  
  test("test3") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,8))(cp)
	  val b = x.isIn(Set(1,5,7))
	  b.isBound should be(false)
  }

  
  test("test4") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,8))(cp)
	  val b = x.isIn(Set(1,5,7))
	  b.isBound should be(false)
	  cp.add(x === 5)
	  b.isTrue should be(true)
  }
  
  test("test5") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,8))(cp)
	  val b = x.isIn(Set(1,5,7))
	  b.isBound should be(false)
	  cp.add(x === 8)
	  b.isFalse should be(true)
  }
  
  test("test6") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,8))(cp)
	  val b = x.isIn(Set(1,5,7))
	  cp.add(b)
	  x.isBoundTo(5) should be(true)
  }
  
  test("test7") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,8))(cp)
	  val b = x.isIn(Set(1,5,7))
	  cp.add(!b)
	  x.toSet should be(Set(2,8))
  }   

  test("test8") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,5,7))(cp)
	  val b = x.isIn(Set(1,5,7))
	  cp.add(x !== 2)
	  b.isTrue should be(true)
  }
  
  test("test9") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,3,7))(cp)
	  val b = x.isIn(Set(1,5,7))
	  cp.add(x !== 7)
	  b.isFalse should be(true)
  }
  
  test("test10") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,3,7,9))(cp)
	  val b = x.isIn(Set(2,3,6,8))
	  cp.add(x !== 2)
	  cp.add(x !== 3)
	  b.isFalse should be(true)
  } 
  
  test("test11") { 
	  val cp = CPSolver()
	  val x = CPIntVar(Set(2,3,7,9))(cp)
	  val b = x.isIn(Set(2,3,6,8))
	  cp.add(x !== 7)
	  cp.add(x !== 9)
	  b.isTrue should be(true)
  }   

}
