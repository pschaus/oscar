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
import oscar.cp.core.CPPropagStrength


class TestGCCVariableBounds extends TestSuite {

  def nbSol(domX: Array[Set[Int]], values: Range, min: Array[Int], max: Array[Int], s: CPPropagStrength): (Int, Int, Int) = {
    var nbSol = 0
    val cp = CPSolver()

    val X = Array.tabulate(domX.size)(i => CPIntVar(domX(i))(cp))
    val cards = (values zip (min zip max).map{case(a,b) => CPIntVar(a to b)(cp)}).toMap

    try {
      cp.add(gcc(X, cards), s)

    } catch {
      case e: NoSolutionException => return (0, 0, 0)
    }
    cp.search {
      binaryStatic(X)
    } onSolution {
      /*
      println("strength:"+s)
      println(X.mkString(","))
      println("range:"+values)
      println(cards.mkString(","))
      */
      assert(cards.values.forall { o => o.isBound })
      nbSol += 1
    }
    
    val stat = cp.start()
    (nbSol, stat.nFails, stat.nNodes)
  
  }

  var rand = new scala.util.Random(0)
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)-3).toSet

  test("GccBC2") {
    var nbWins = 0
    for (i <- 1 to 200) {
      rand =  new scala.util.Random(i)
      
      val nbVars = 8

      val domVars = Array.fill(nbVars)(randomDom(size = nbVars))

      val min = domVars.flatten.min
      val max = domVars.flatten.max

      
      var cardMin = (for (v <- min to max) yield rand.nextInt(3)).toArray
      var cardMax = cardMin.map(v => v+rand.nextInt(3))

      var (nSol1, nSol2, nSol3) = (0, 0, 0)
      var (bkt1, bkt2, bkt3) = (0, 0, 0)
      var (nNode1, nNode2, nNode3) = (0, 0, 0)
      val t1 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, min to max, cardMin,cardMax, CPPropagStrength.Weak)
        nSol1 = a
        bkt1 = b
        nNode1 = c
      }
      val t2 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, min to max, cardMin,cardMax, CPPropagStrength.Medium)
        nSol2 = a
        bkt2 = b
        nNode2 = c
      }
      val t3 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, min to max, cardMin,cardMax, CPPropagStrength.Strong)
        nSol3 = a
        bkt3 = b
        nNode3 = c
      }

      //println(nSol1)
      nSol1 should equal(nSol2)
      nSol1 should equal(nSol3)

        

    }
  }

  test("Magic sequence") {
    for (n <- /*1*/ 3 to 20) {
      var nSols = -1
      for (strength <- Array(Weak, Medium, Strong)) {
        val cp = CPSolver()

        val x = Array.fill(n)(CPIntVar(0 until n)(cp))
        val allValues = Array.tabulate(n)(i => (i, x(i)))

        cp.post(gcc(x, allValues), strength)

        cp.search {
          binaryStatic(x)
        }

        /*cp.onSolution {
          println("Solution:")
          println("x: " + x.mkString(" "))
        }*/

        val sols = cp.start().nSols
        //if (nSols != -1)
        //  sols should equal(nSols)
        nSols = sols
      }
      //println("Magic sequence of length " + n + ": " + nSols)
    }
  }
}
