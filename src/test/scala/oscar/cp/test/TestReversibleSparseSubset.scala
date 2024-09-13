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
import oscar.cp._
import collection.immutable.SortedSet
import oscar.algo.reversible.ReversibleSparseSubset
import org.scalatest.Matchers



/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestReversibleSparseSubset extends FunSuite with Matchers  {


  
    test("Test 1") {
   
		
		val cp = CPSolver()
    	
		val s = new ReversibleSparseSubset(cp,5, 10)
		s.requires(8)
		s.possibleSet should be(Set(5,6,7,8,9,10))
		s.requiredSet should be(Set(8))
		(5 to 10).forall(s.isPossible(_)) should be(true)
		(4 to 12).forall(!s.isRequired(_)) should be(false)
		s.isPossible(4) should be(false)
		s.isPossible(11) should be(false)
		
		s.excludes(9)
		s.possibleSet should be(Set(5,6,7,8,10))
		s.requiredSet should be(Set(8))
		s.isPossible(9) should be(false)
		

		
		s.requires(5)
	    s.possibleSet should be(Set(5,6,7,8,10))
		s.requiredSet should be(Set(5,8))

		
		s.excludes(6)
		s.excludes(7)
		s.excludes(10)
		s.possibleSet should be(s.requiredSet)
		s.possibleSet should be(Set(5,8))	
		

		intercept[RuntimeException] {
			s.requires(9)
		}
		intercept[RuntimeException] {
			s.excludes(5)
		}		
    
  } 
    
  test("Test 2") {
  	
		val cp = CPSolver()
    	
		val s = new ReversibleSparseSubset(cp,5, 10)
		(5 to 10).forall(!s.isRequired(_)) should be(true)
		s.requires(8)
		s.possibleSet should be(Set(5,6,7,8,9,10))
		s.requiredSet should be(Set(8))
		(5 to 10).forall(s.isPossible(_)) should be(true)
		(4 to 12).forall(!s.isRequired(_)) should be(false)
		s.isPossible(4) should be(false)
		s.isPossible(11) should be(false)
		
		s.excludes(9)
		s.possibleSet should be(Set(5,6,7,8,10))
		s.requiredSet should be(Set(8))
		s.isPossible(9) should be(false)
		
		cp.pushState()
		
		s.requires(5)
	    s.possibleSet should be(Set(5,6,7,8,10))
		s.requiredSet should be(Set(5,8))
		
		cp.pushState()
		
		s.excludes(6)
		s.excludes(7)
		s.excludes(10)
		s.possibleSet should be(s.requiredSet)
		s.possibleSet should be(Set(5,8))	
		
		cp.pop()
		

	    s.possibleSet should be(Set(5,6,7,8,10))
		s.requiredSet should be(Set(5,8))
		
		cp.pop()
		
	    s.possibleSet should be(Set(5,6,7,8,10))
		s.requiredSet should be(Set(8))
		s.isPossible(9) should be(false)	
    
  } 
  

  test("Test 3") {
   	
		val cp = CPSolver()
    	
		val s = new ReversibleSparseSubset(cp,-2, 2)
		s.possibleSet should be(Set(-2,-1,0,1,2))
		s.requiredSet should be(Set())
		(-2 to 2).forall(s.isPossible(_)) should be(true)
		(-2 to 2).forall(!s.isRequired(_)) should be(true)
		s.isPossible(-3) should be(false)
		s.isPossible(3) should be(false)
	

		s.possibleSet should be(Set(-2,-1,0,1,2))
		s.possibleNotRequiredValues.toSet should be(Set(-2,-1,0,1,2))
		
		s.excludes(0)
		s.requires(-2)
		s.possibleSet should be(Set(-2,-1,1,2))
		s.possibleNotRequiredValues.toSet should be(Set(-1,1,2))
		s.requiredSet should be(Set(-2))
		s.isPossible(-2) should be(true)

  }  
  
  test("Test 4") {
   	
		val cp = CPSolver()
    	
		val s = ReversibleSparseSubset(cp,Set(-2, 2))
		s.possibleSet should be(Set(-2,2))
		s.requiredSet should be(Set())
		s.requires(2)
		s.possibleNotRequiredValues.toSet should be(Set(-2))
		s.possibleSet should be(Set(-2,2))
		s.requiredSet should be(Set(2))

  } 
  
  
  
    test("Test 5") {
   
		
		val cp = CPSolver()
    	
		val s = ReversibleSparseSubset(cp,Set(-6,-10,1,5,9,11))
		
		intercept[RuntimeException] {
			s.requires(8)
		}
		
		s.requires(-10)
		s.requires(9)
		s.excludesAll()
		
		
		s.possibleSet should be(Set(-10,9))
		s.requiredSet should be(Set(-10,9))
		

    
  }  
    
    test("Test 6") {
   
		
		val cp = CPSolver()
		val s = ReversibleSparseSubset(cp,Set(-6,-10,1,5,9,11))
		
		s.excludes(1)
		s.excludes(5)
		s.excludes(9)
		s.requires(-10)
		s.requiresAll()
	    s.possibleSet should be(Set(-6,-10,11))
		s.requiredSet should be(Set(-6,-10,11))

    
  }    
  

  


}
