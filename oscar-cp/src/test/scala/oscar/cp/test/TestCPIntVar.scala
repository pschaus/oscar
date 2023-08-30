/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.test


import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp._
import oscar.cp.core.variables.CPVar
import oscar.cp.testUtils._

class TestCPIntVar extends TestSuite {

  test("Test1 : Median") {

    val cp = CPSolver()

    val a = CPIntVar(Array(0, 1, 2, 3, 4))(cp)

    a.median should be(2)

    a.removeValue(1)

    a.median should be(3)

    a.removeValue(4)

    a.median should be(2)

  }

  test("Iterator1") {
    val cp = CPSolver()
    val x = CPIntVar(Set(0, 1, 3, 2))(cp)
    x.toSet should be(Set(0, 1, 2, 3))
  }

  test("Iterator2") {
    val cp = CPSolver()
    val x = CPIntVar(Set(0, 1, 3, 2))(cp) + 1
    x.toSet should be(Set(1, 2, 3, 4))
  }

  test("Iterator3a") {
    val cp = CPSolver()
    val x = CPIntVar(Set(1, 3))(cp)
    x.toSet should be(Set(1, 3))
  }

  test("Iterator3") {
    val cp = CPSolver()
    val x = CPIntVar(Set(1, 3))(cp) - 1
    x.toSet should be(Set(0, 2))
  }

  test("Iterator4") {
    val cp = CPSolver()
    val x = CPIntVar(Set(1, 3, 5))(cp) - 1
    cp.add(x !== 2)
    x.toSet should be(Set(0, 4))
  }

  test("Iterator5") {
    val cp = CPSolver()
    val x = CPIntVar(1 to 5)(cp) - 1

    x.toSet should be(Set(0, 1, 2, 3, 4))
    cp.add(x !== 2)
    x.toSet should be(Set(0, 1, 3, 4))
  }

  test("isBound test - var does get bound") {
    val cp = CPSolver()
    val a = CPIntVar(Array(10, 20, 30))(cp)
    a.isBound should be(false)
    cp.add(a === 10)
    a.isBound should be(true)
  }

  test("isBound test - var doesn't get bound") {
    val cp = CPSolver()
    val a = CPIntVar(Array(10, 20, 30))(cp)
    a.isBound should be(false)

    assertThrows[NoSolutionException] {
      cp.add(a < 10)
    }
  }

  test("min max size methods test") {

    val cp = CPSolver()

    var x = CPIntVar(-2 to 4)(cp)

    x.min should be(-2)
    x.max should be(4)

    for (i <- 0 to 5) {

      cp.pushState()
      cp.add(x !== 0)
      cp.add(x !== 2)

      x.size should be(5)
      x.min should be(-2)
      x.max should be(4)

      cp.add(x !== -1)
      cp.add(x !== -2)

      x.size should be(3)
      x.min should be(1)
      x.max should be(4)

      cp.pop()

      x.size should be(7)
      x.min should be(-2)
      x.max should be(4)
    }

  }

  test("domain iterator 1") {
    val cp = CPSolver()
    var x = CPIntVar(Set(1, 3, 5))(cp)

    val d = x.iterator
    x.removeValue(d.next)
    x.removeValue(d.next)
    assert(isInconsistent(x.removeValue(d.next) ))
    d.hasNext should be(false)
  }

  test("domain iterator 2") {
    val cp = CPSolver()
    var x = CPIntVar(Set(1, 3, 5, 11))(cp)
    val initVals = x.toSet
    val d = x.iterator
    val removed = (for (i <- 1 to x.size - 1) yield {
      val v = d.next
      x.removeValue(v)
      v
    }).toSet

    removed.size should be(3)
    x.size should be(1)
    x.isBound should be(true)
    (initVals -- removed).contains(x.value) should be(true)
  }

  test("domain iterator 3") {
    val cp = CPSolver()
    var x = CPIntVar(Set(1, 3, 5, 11, 15, 17))(cp)
    x.removeValue(15)
    val d = x.iterator
    val toRemove = Set(3, 11, 17)
    d.foreach(v => {
      if (toRemove(v)) x.removeValue(v)
    })

    x.toSet should be(Set(1, 5))
  }

  test("test propagate call") {
    var propagCalled = 0
    var valBindCalled = 0
    var i = 0
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {

      override def setup(l: CPPropagStrength): Unit = {
        X.callPropagateWhenBind(this)
        X.callValBindWhenBind(this)
      }

      override def propagate(): Unit = {
        i += 1
        propagCalled = i
      }

      override def valBind(x: CPIntVar): Unit = {
        i += 1
        valBindCalled = i
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp)
    cp.add(new MyCons(x))
    cp.add(x === 3)
    valBindCalled should be(1)
    propagCalled should be(2)

  }

}
