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

/**
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestSetCard extends TestSuite  {


  test("Test SetCard 1") {
    var nbSol = 0
    val cp = CPSolver()
    var x = new CPSetVar(cp, 1 , 5)
    cp.post(x.card <= 2)
    cp.post(x.card >= 1)
    cp.search {
      binary(x)
    }
    cp.start().nSols should be(15)
  }
  
  test("Test SetCard 2") {
    val cp = CPSolver()
    val x = CPSetVar(Set(1, 3, 4), Set(1, 3))(cp)
    cp.post(x.card === 2)
    x.isBound should be(true)
  }
  
  test("Test SetCard 3") {
    val cp = CPSolver()
    val x = CPSetVar(Set(1, 3, 4), Set(1, 3))(cp)
    cp.post(x.card === 3)
    x.isBound should be(true)
    x.value.size should be(3)
  } 
  
  
  test("Test SetCard 4") {
    val cp = CPSolver()
    val x = CPSetVar(Set(1, 3, 4), Set(1, 3))(cp)
    val c = CPIntVar(-100,100)(cp)
    cp.post(x.card === c)
    c.min should be(2)
    c.max should be(3)
  }  
  
  
}
  
