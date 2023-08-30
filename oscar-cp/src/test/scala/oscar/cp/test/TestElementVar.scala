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
import oscar.cp.constraints.ElementCst2D
import org.scalatest.Matchers

/**
 * Test on Element Var Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestElementVar extends FunSuite with Matchers {

  // --------- gac element var ----------

  test("Test Element Var AC 1") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(1 to 2)(cp), CPIntVar(1 to 2)(cp), CPIntVar(1 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Strong)
    z.min should be(1)
    z.max should be(2)
  }
  
  test("Test Element Var AC 2") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(Set(1, 3))(cp), CPIntVar(Set(4))(cp), CPIntVar(Set(1, 4))(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Strong)
    z.hasValue(2) should be(false)
    z.min should be(1)
    z.max should be(4)

    cp.add(y(0) !== 3)
    z.hasValue(3) should be(false)

    cp.add(z >= 2)
    x.hasValue(0) should be(false)
    z.isBoundTo(4) should be(true)
  }

  test("Test Element Var AC 3") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(Set(1, 3))(cp), CPIntVar(Set(4))(cp), CPIntVar(Set(1, 5))(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Strong)

    cp.add(x !== 1)

    z.hasValue(4) should be(false)
    z.size should be(3) // 1,3,5

  }

  test("Test Element Var AC 4") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(Set(1, 3))(cp), CPIntVar(Set(4))(cp), CPIntVar(Set(1, 5))(cp))
    val z = CPIntVar(-20 to 10000)(cp)

    cp.add(elementVar(y, x, z), Strong)

    cp.add((y(0) ?!== 1) && (y(2) ?!== 1))
    z.min should be(3)
    z.max should be(5)

  }

  test("Test Element Var AC 5") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(1 to 3)(cp), CPIntVar(2 to 2)(cp), CPIntVar(2 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Strong)

    z.min should be(1)
    cp.add(z < 2)
    x.isBound should be(true)
    x.value should be(0)
    y(0).isBound should be(true)
    y(0).value should be(1)
  }

  test("Test Element Var AC 6") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(3, 4)(cp), CPIntVar(2 to 2)(cp), CPIntVar(2 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Strong)

    cp.add(z === 2)

    x.min should be(1)
    x.max should be(2)
  }

  //1from z: [1, 2, 3, 5] y= [0, 2, 3, 4, 5], [0, 2, 3, 4, 5], [0, 2, 3, 4, 5], [0, 2, 3, 4, 5], 1, [0, 2, 3, 4, 5]of  1..5= [1, 2, 3, 5]

  test("Test Element Var AC 7") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(Set(0, 3, 5))(cp), CPIntVar(0 to 0)(cp), CPIntVar(Set(1, 3, 5))(cp))
    val z = CPIntVar(0 to 5)(cp)

    cp.add(elementVar(y, x, z), Strong)

    x.removeValue(0)
    y(0).removeValue(0)
    cp.add(x >= -1)
    //println("y:" + y.mkString(",") + " x:" + x + z)
    z.hasValue(0) should be(true)
  }

  test("Test Element Var AC8") {
    val len = 6
    val cp = CPSolver()
    val x = Array.tabulate(len)(i => CPIntVar(0 to len - 1)(cp))
    val z = Array.tabulate(len)(i => CPIntVar(0 to len - 1)(cp))

    cp.add(allDifferent(x), Strong)
    cp.add(allDifferent(z), Strong)


    for (i <- 1 until len) {
      cp.add(elementVar(x, z(i - 1), z(i)), Strong)
    }
    
    
    cp.add(z(len - 1) === 0)
    cp.add(x(4) === 1)
    cp.add(x(0) === 4)
    cp.add(x(1) === 5)
    cp.add(x(3) === 2)
    cp.add(x(5) === 3)
    cp.add(x(2) === 0)
    cp.isFailed should be(false)

  }
  
  test("Test Element Var AC9") {

    val cp = CPSolver()

    val z = CPIntVar(1, 2)(cp)
    val x = CPIntVar(0 to 3)(cp)
    val tab = Array(CPIntVar(1 to 2)(cp), CPIntVar(2 to 3)(cp))

    cp.add(elementVar(tab, x, z),Strong)

    x.min should be(0)
    x.max should be(1)

    cp.add(z !== 2)

    x.isBound should be(true)
    tab(1).min should be(2)
    tab(1).size should be(2)
    tab(0).value should be(1)
  }

  test("Test Element Var AC10") {

    val cp = CPSolver()

    val z = CPIntVar(0 to 2)(cp)
    val x = CPIntVar(0 to 2)(cp)
    val tab = Array(CPIntVar(0 to 2)(cp), CPIntVar(0 to 2)(cp), CPIntVar(0 to 2)(cp))

    cp.add(elementVar(tab, x, z),Strong)

    x.min should be(0)
    x.max should be(2)

    cp.add(tab(0) !== 0)
    cp.add(tab(1) !== 1)
    cp.add(tab(2) !== 2)

    x.min should be(0)
    x.max should be(2)

    cp.add(tab(0) === 1)
    cp.add(tab(2) !== 1)

    x.min should be(0)
    x.max should be(2)

    cp.add(z !== 1)

    x.min should be(1)
    x.max should be(2)

    cp.add(z !== 2)

    x.min should be(1)
    x.max should be(2)

    cp.add(tab(1) !== 0)

    x.isBound should be(true)
  }
  
  test("Test Element Var AC11") {

    val cp = CPSolver()

    val z = CPIntVar(1 to 3)(cp)
    val x = CPIntVar(0 to 3)(cp)
    val tab = Array(CPIntVar(1 to 2)(cp), CPIntVar(2 to 3)(cp), CPIntVar(2 to 3)(cp))

    cp.add(elementVar(tab, x, z),Strong)

    x.min should be(0)
    x.max should be(2)
    
    cp.add(x !== 2)
    
    x.min should be(0)
    x.max should be(1)
    z.min should be(1)
    z.max should be(3)
    
    cp.add(tab(2) !== 3)
    
    x.min should be(0)
    x.max should be(1)
    z.min should be(1)
    z.max should be(3)
  }
  
  test("Test Element Var AC12") {

    val cp = CPSolver()

    val z = CPIntVar(1 to 3)(cp)
    val x = CPIntVar(0 to 1)(cp)
    val tab = Array(CPIntVar(1 to 2)(cp), CPIntVar(3)(cp))

    cp.add(elementVar(tab, x, z),Strong)
    
    x.min should be(0)
    x.max should be(1)
    z.min should be(1)
    z.max should be(3)
    
    cp.add(tab(0) !== 1)
    
    x.min should be(0)
    x.max should be(1)
  }
  

  
  // --------- bound consistent elementVar var ----------

  test("Test Element Var BC 1") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(1 to 2)(cp), CPIntVar(1 to 2)(cp), CPIntVar(1 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Weak)
    z.min should be(1)
    z.max should be(2)
    x.min should be(0)
    x.max should be(2)
  }

  test("Test Element Var BC 2") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(1 to 3)(cp), CPIntVar(2 to 2)(cp), CPIntVar(2 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Weak)

    z.min should be(1)
    cp.add(x !== 0)
    z.min should be(2)
  }

  test("Test Element Var BC 3") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(1 to 3)(cp), CPIntVar(2 to 2)(cp), CPIntVar(2 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Weak)

    z.min should be(1)
    cp.add(z < 2)

    x.isBound should be(true)
    x.value should be(0)
    y(0).isBound should be(true)
    y(0).value should be(1)

  }

  test("Test Element Var BC 4") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(1 to 3)(cp), CPIntVar(2 to 2)(cp), CPIntVar(2 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Weak)

    cp.add(z === 2)
    x.min should be(0)
    x.max should be(2)
  }

  test("Test Element Var BC 5") {
    val cp = CPSolver()
    val x = CPIntVar(-3 to 10)(cp)
    val y = Array(CPIntVar(3, 4)(cp), CPIntVar(2 to 2)(cp), CPIntVar(2 to 2)(cp))
    val z = CPIntVar(-20 to 100)(cp)

    cp.add(elementVar(y, x, z), Weak)

    cp.add(z === 2)

    x.min should be(1)
    x.max should be(2)
  }
  
}
