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
import oscar.cp.constraints.Permutation

class TestPermutation extends TestSuite {

	test("Test Permutation 1") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPIntVar(-6 to 6)(cp))
		val y = Array.fill(4)(CPIntVar(-6 to 6)(cp))
		
		cp.add(x(0) >= 2)
		cp.add(new Permutation(x,y))

		
		y(0).hasValue(0) should be(false)
		y(1).hasValue(0) should be(false)
		y(2).hasValue(0) should be(true)
		
	}
	
	test("Test Permutation 2") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPIntVar(-6 to 6)(cp))
		val y = Array.fill(4)(CPIntVar(-6 to 6)(cp))

		cp.add(new Permutation(x,y))

		x.forall(_.min == 0) should be(true)
		y.forall(_.min == 0) should be(true)
		x.forall(_.max == 3) should be(true)
		y.forall(_.max == 3) should be(true)
		
		cp.add(x(0) >= 2)
		
		
		y(0).hasValue(0) should be(false)
		y(1).hasValue(0) should be(false)
		y(2).hasValue(0) should be(true)
		
	}
	
	test("Test Permutation 4") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPIntVar(-6 to 6)(cp))
		val y = Array.fill(4)(CPIntVar(-6 to 6)(cp))

		cp.add(new Permutation(x,y))
		cp.add(x(2) === 2)
		
		
		y(2).isBound should be(true)
		y(2).value should be(2)
		y(3).hasValue(2) should be(false)
		
		cp.add(x(0) === 3)
	    y(3).isBound should be(true)
		y(3).value should be(0)
		
	}		
	
	test("Test Permutation 5") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPIntVar(-6 to 6)(cp))
		val y = Array.fill(4)(CPIntVar(-6 to 6)(cp))

		cp.add(new Permutation(x,y))
		cp.add(y(2) === 2)
		
		
		x(2).isBound should be(true)
		x(2).value should be(2)
		x(3).hasValue(2) should be(false)
		
	}	

}
