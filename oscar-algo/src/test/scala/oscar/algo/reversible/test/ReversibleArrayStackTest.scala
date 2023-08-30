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

package oscar.algo.reversible.test


import org.scalatest.FunSuite
import oscar.algo.reversible._
import oscar.algo.reversible.ReversibleArrayStack

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleArrayStackTest extends FunSuite {

  test("test ReversibleArrayStack 1") {

    val rc = new ReversibleContextImpl()
    val s = new ReversibleArrayStack[Int](rc)
    
    s.push(1)
    s.push(2)
    s.push(3)

    assert(s(0) == 1)
    assert(s(1) == 2)
    assert(s(2) == 3)
    assert(s.size == 3)
    
    rc.pushState()
    
    s.push(4)
    s.push(5)
    s.push(6) 
    
    assert(s.size == 6)
    assert(s(5) == 6)
    
    s.clear()
    
    assert(s.size == 0)
    
    rc.pop()
    
    assert(s(0) == 1)
    assert(s(1) == 2)
    assert(s(2) == 3)
    assert(s.size == 3)    
  }
}

