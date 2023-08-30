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
import oscar.algo.debug
import oscar.cp.constraints.{AllDiffAC, AllDiffFWC, CountingBasedAllDifferent}


class TestCountingBasedAllDifferent extends TestSuite  {


  test("CountingBasedAllDifferent : small") {
    implicit val cp = CPSolver()
    val x = Array.fill(2)(CPIntVar(0 to 1))
    post(new CountingBasedAllDifferent(x))
    search(binaryStatic(x))
    val stat = start()
    assert(stat.nSols == 2)
  }


  test("CountingBasedAllDifferent : incremental number of processed variables") {
    implicit val cp = CPSolver()
    val x = Array(CPIntVar(-1 to 0), CPIntVar(0 to 2), CPIntVar(2))
    post(new CountingBasedAllDifferent(x))
    search(binaryStatic(x))
    val stat = start()
    assert(stat.nSols == 3)
  }

  test("CountingBasedAllDifferent : incremental number of processed variables 2") {
    implicit val cp = CPSolver()
    val x = Array(CPIntVar(0 to 1), CPIntVar(3 to 4), CPIntVar(1 to 3), CPIntVar(2))
    post(new CountingBasedAllDifferent(x))
    search(binaryStatic(x))
    val stat = start()
    assert(stat.nSols == 4)
  }



  
  val rand = new scala.util.Random(0)
  
  def randomDom(size: Int) = {
    //Array.fill(size)(rand.nextInt(size)).toSet

    val offset = rand.nextInt(size) // to get some negative values

    val min = rand.nextInt(size) - offset
    val max = (min + rand.nextInt(3)) max(size-1) - offset
    (min to max).toSet
  }

  test("CountingBasedAllDifferent : random") {
    for (i <- 0 until 200) {
      if (i == 5) debug = true
      val cp = CPSolver()
      val n = 6
      val x = Array.tabulate(n)(i => CPIntVar(randomDom(n+1))(cp))

      cp.pushState()

      cp.search(binaryStatic(x))

      val stat1 = cp.startSubjectTo() {
        cp.add(new AllDiffFWC(x))
      }

      val stat2 = cp.startSubjectTo() {
        cp.post(new CountingBasedAllDifferent(x))
      }

      val stat3 = cp.startSubjectTo() {
        cp.post(new AllDiffAC(x))
      }

      assert(stat1.nSols == stat2.nSols)
      assert(stat1.nFails >= stat2.nFails)
      assert(stat2.nFails >= stat3.nFails)
    }
  }
}
