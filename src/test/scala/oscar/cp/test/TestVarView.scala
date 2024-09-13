/** *****************************************************************************
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
 * *****************************************************************************/
package oscar.cp.test

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import oscar.cp.testUtils.TestSuite


class TestVarView extends TestSuite {


  test("Table Var View 1") {


    class ViewCons(val X: CPIntVar) extends Constraint(X.store, "TestView") {

      var valRemove = true

      override def setup(l: CPPropagStrength): Unit = {
        X.callValRemoveWhenValueIsRemoved(this)
        X.callValBindWhenBind(this)
        X.callUpdateBoundsWhenBoundsChange(this)
      }

      override def valRemove(x: CPIntVar, v: Int): Unit = {
        val eq: Boolean = x == X
        eq should equal(true)
        if (valRemove) {
          v should be(0)
          valRemove = false
        }
      }

      override def valBind(x: CPIntVar): Unit = {
        val eq: Boolean = x == X
        eq should equal(true)
        x.value should be(-2)
      }


      override def updateBounds(x: CPIntVar): Unit = {
        val eq: Boolean = x == X
        eq should equal(true)
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }
    val cp = CPSolver()
    var x = CPIntVar(-2 to 4)(cp)
    val y = x + 5 + 2 - 3 - 5 // y = x-1 so its domain should be -3..3
    cp.add(new ViewCons(y))
    cp.add(y !== 0)
    cp.add(y >= -2)
    y.max should be(3)
    y.min should be(-2)
    cp.add(y <= -2) // now it's bind to -2
    y.value should be(-2)
  }


  // same tests but with l1 indexes methods
  test("Table Var View 2") {
    val cp = CPSolver()
    var x = CPIntVar(-2 to 4)(cp)

    class ViewCons(val X: CPIntVar) extends Constraint(X.store, "TestView") {
      override def associatedVars(): Iterable[CPVar] = ???
      var valRemove = true

      override def setup(l: CPPropagStrength): Unit = {
        X.callValRemoveIdxWhenValueIsRemoved(this, -1)
        X.callValBindIdxWhenBind(this, -1)
        X.callUpdateBoundsIdxWhenBoundsChange(this, -1)
      }

      override def valRemoveIdx(x: CPIntVar, idx: Int, v: Int): Unit = {
        val eq: Boolean = x == X
        eq should equal(true)
        if (valRemove) {
          v should be(0)
          valRemove = false
        }
      }

      override def valBindIdx(x: CPIntVar, idx: Int): Unit = {
        val eq: Boolean = x == X
        eq should equal(true)
        x.value should be(-2)
      }

      override def updateBoundsIdx(x: CPIntVar, idx: Int): Unit = {
        val eq: Boolean = x == X
        eq should equal(true)
      }

    }

    val y = x + 5 + 2 - 3 - 5 // y = x-1 so its domain should be -3..3
    cp.add(new ViewCons(y))
    cp.add(y !== 0)
    cp.add(y >= -2)
    y.max should be(3)
    y.min should be(-2)
    cp.add(y <= -2) // now it's bind to -2
    y.value should be(-2)
  }


  test("Table Var View 3") {
    val cp = CPSolver()
    val x = CPIntVar(0 to 3)(cp)
    val minusx = -x
    minusx.min should be(-3)
    minusx.max should be(0)
    minusx.removeValue(-1)
    x.hasValue(1) should be(false)
    minusx.valueBefore(-1) should be(-2)
    minusx.valueAfter(-3) should be(-2)
    minusx.updateMin(-2)
    minusx.min should be(-2)
    x.max should be(2)
    minusx.hasValue(0) should be(true)
    minusx.updateMax(-1)
    minusx.hasValue(0) should be(false)
  }


}
