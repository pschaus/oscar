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
import oscar.algo.Inconsistency
import oscar.cp.testUtils.TestSuite
import oscar.cp.constraints._
import oscar.cp._
import oscar.cp.core.CPPropagStrength


class TestCumulativeAlternative extends TestSuite  {

  test("issue76") {
    implicit val solver = new CPSolver()
    val ss = Array(CPIntVar(0 to 20))
    val ds = Array(CPIntVar(6))
    val es = Array(CPIntVar(0 to 20))
    val dems = Array(CPIntVar(6))
    val rs = Array(CPIntVar(0))
    add(ss(0) + ds(0) === es(0))
    intercept[NoSolutionException] {
    	add(maxCumulativeResource(ss, ds, es, dems, rs, CPIntVar(3), 0), CPPropagStrength.Strong)
    }
  }
  
  test("issue76 a") {
    implicit val solver = new CPSolver()
    val ss = Array(CPIntVar(0 to 20))
    val ds = Array(CPIntVar(6))
    val es = Array(CPIntVar(0 to 20))
    val dems = Array(CPIntVar(6))
    val rs = Array(CPIntVar(0))
    add(ss(0) + ds(0) === es(0))
    val capa = CPIntVar(0 to 6)
    add(maxCumulativeResource(ss, ds, es, dems, rs, capa, 0), CPPropagStrength.Strong)
    intercept[NoSolutionException] {
      add(capa < 6)
    }
  }
  
  test("issue76 b") {
    implicit val solver = new CPSolver()
    val ss = Array(CPIntVar(0 to 20))
    val ds = Array(CPIntVar(6))
    val es = Array(CPIntVar(0 to 20))
    val dems = Array(CPIntVar(6))
    val rs = Array(CPIntVar(0 to 1))
    add(ss(0) + ds(0) === es(0))
    val capa = CPIntVar(0 to 6)
    add(maxCumulativeResource(ss, ds, es, dems, rs, capa, 0), CPPropagStrength.Strong)
    add(capa < 6)
    rs(0).isBoundTo(1) should be(true)
  }  


}
