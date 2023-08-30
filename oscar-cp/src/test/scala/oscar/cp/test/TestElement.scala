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
import oscar.cp.modeling._
import oscar.cp.testUtils.TestSuite
import oscar.cp.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

/**
 * Test on Element Var Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestElement extends TestSuite {

  class ElementChecker(val y: Array[CPIntVar], val x: CPIntVar, val z: CPIntVar) extends Constraint(y(0).store, "ElementChecker") {
    override def setup(l: CPPropagStrength): Unit = {
      
      for (yvar <- y) yvar.callPropagateWhenBind(this)
      x.callPropagateWhenBind(this)
      z.callPropagateWhenBind(this)
      /*
      x.callPropagateWhenDomainChanges(this)
      z.callPropagateWhenDomainChanges(this)
      for (yvar <- y) yvar.callPropagateWhenDomainChanges(this)*/
      return propagate()
    }
    override def propagate(): Unit = {
      if (x.isBound) {
        if (x.min < 0 || x.min >= y.length) {
           throw Inconsistency
        }
        assert(x.max >= 0)
        if (z.isBound && y(x.min).isBound) {
          if (y(x.min).min == z.min) {
             deactivate()
          }
          else
            throw Inconsistency
        }
      }
      else {
        assert(!(x.isBound && y(x.min).isBound && z.isBound))
      }
    }
    override def associatedVars(): Iterable[CPVar] = ???
  }


  test("Test Element Var random tests") {

    val rand = new scala.util.Random(0)

    def randomDom(n: Int, min: Int, max: Int): Set[Int] = {
      (for (i <- 0 until n) yield (rand.nextInt(max - min + 1) + min)).toSet
    }

    for (t <- 0 until 1000) {
      val cp = CPSolver()
      val nIndices = 5
      val y = Array.fill(nIndices)(CPIntVar(randomDom(3, 3, 9))(cp))
      val z = CPIntVar(randomDom(3, 3, 8))(cp)
      val x = CPIntVar(randomDom(5,-1, nIndices + 2))(cp)
      val allVars = (y :+ z :+ x).asInstanceOf[Array[CPIntVar]]
      //val allVars = (Array(z) ++ y :+ x).asInstanceOf[Array[CPIntVar]]
      //val allVars = scala.util.Random.shuffle((Array(z) ++ y :+ x).asInstanceOf[Array[CPIntVar]])

      cp.post(allDifferent(Array(y(1), y(3), x)),CPPropagStrength.Medium)
      if (!cp.isFailed) {
        cp.search(binaryStaticIdx(allVars,i => allVars(i).min))
        val stat1 = cp.startSubjectTo() {
          cp.add(elementVar(y, x, z), CPPropagStrength.Medium)
        }
        val stat2 = cp.startSubjectTo() {
          cp.add(elementVar(y, x, z), CPPropagStrength.Strong)
        }

        val stat3 = cp.startSubjectTo() {
          cp.add(new ElementChecker(y, x, z))
        }
        
        assert(stat1.nSols == stat2.nSols)
        assert(stat3.nSols == stat2.nSols)
      }

    }

  }
 
  
 
  
  
}
