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


import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.testUtils.TestSuite

class TestNoSolutionException extends TestSuite {
	//TODO Guillaume convert to TestInconsistency
	/*test("test1: using add outside subjectTo allows NoSolutionExceptions") {
	  val cp = CPSolver()
	  val x = CPIntVar(Array(10, 20, 30))(cp)
	  
	  intercept[NoSolutionException]{ intercept[Inconsistency] {
	    cp.add(x < 10)
	  }  }
	  
	  cp.isFailed should be(true)
	}
	
	test("test3: using post outside subjectTo shouldn't generate NoSolutionExceptions") {
	  val cp = CPSolver()
	  val x = CPIntVar(Array(10, 20, 30))(cp)
	  
	  postAndCheckFailure(cp, x < 10)
	  cp.isFailed should be(true)
	}
	
	test("test4: using post inside subjectTo shouldn't generate NoSolutionExceptions") {
	  val cp = CPSolver()
	  val x = CPIntVar(Array(10, 20, 30))(cp)

		postAndCheckFailure(cp, x < 10)
	  cp.isFailed should be(true)
	}*/
}
