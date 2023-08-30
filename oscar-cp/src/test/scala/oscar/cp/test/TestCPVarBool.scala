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
import oscar.cp._

class TestCPBoolVar extends TestSuite {
	
	test("Test1") {

		val cp = CPSolver()
		
		val x = CPBoolVar()(cp)

		x.isEmpty should be(false)
		x.size should be(2)
		
		x.removeValue(0)
		
		x.isEmpty should be(false)
		x.size should be(1)
		
		x.removeValue(0)
		
		x.isEmpty should be(false)
		x.size should be(1)		

		isInconsistent(x.removeValue(1)) should be(true)
		
		x.isEmpty should be(true)
		x.size should be(0)
	}

	/*
	test("Test2") {

		val cp = CPSolver()
		
		val x = CPBoolVar(cp)

		x.isEmpty should be(false)
		x.size should be(2)
		
		x.removeValue(1)
		
		x.isEmpty should be(false)
		x.size should be(1)
		
		x.removeValue(0) should be(Outcome.Failure)
		
		x.isEmpty should be(true)
		x.size should be(0)				
		
	}
	
	test("Test3") {

		val cp = CPSolver()
		
		val x = CPBoolVar(cp)
		
		x.assign(0)
		
		x.isEmpty should be(false)
		x.size should be(1)
		
		x.assign(1) should be(Outcome.Failure)
		
	}
	
	test("Test4") {

		val cp = CPSolver()
		
		val x = CPBoolVar(cp)
		
		x.assign(0)
		
		x.isEmpty should be(false)
		x.size should be(1)
		
		x.assign(0) should be(Outcome.Suspend)
		
	}	
	*/
	



}
