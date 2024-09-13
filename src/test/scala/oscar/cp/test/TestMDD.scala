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

import oscar.cp._
import oscar.cp.constraints.{Automaton, Regular}
import oscar.cp.constraints.mdd.{Mdd4RConstraint, StaticMdd, StaticMddImpl}
import oscar.cp.testUtils.TestSuite

class TestMDD extends TestSuite {


  test("Intersection with itself") {
    val table1 = Array(Array(1, 1, 1), Array(1, 2, 1), Array(2, 2, 1), Array(2, 3, 1))

    val mdd1: StaticMddImpl = StaticMdd.buildMddFromTableRegin(table1, 3)
    val mdd2 = StaticMdd.buildMddFromTableRegin(table1, 3)

    val mdd3 = StaticMdd.intersect(mdd1, mdd2)

    mdd3.numPaths().toString().toInt should equal(table1.size)
    for (tuple <- table1) {
      mdd3.contains(tuple) should be(true)
    }
  }

  test("Intersection with domain so that no change is done to the table") {
    scala.util.Random.setSeed(444719)
    val arities = Array(2, 4, 7, 10, 20)
    val nTuples = Array(100, 200, 500, 1000)
    val maxValues = Array(10, 20, 30)
    for (arity <- arities) {
      for (nTuple <- nTuples) {
        for (maxValue <- maxValues) {
          val table: Array[Array[Int]] = Array.fill(nTuple, arity)(scala.util.Random.nextInt(maxValue))

          val mdd1 = StaticMdd.buildMddFromTableRegin(table, arity)
          val mdd2 = StaticMdd.buildLinearMddFromDomains(Array.fill(arity)(Array.tabulate(maxValue)(i => i)))

          val mdd3 = StaticMdd.intersect(mdd1, mdd2)

          mdd3.numPaths().toString().toInt should equal(mdd1.numPaths().toString().toInt)

          for (tuple <- table) {
            mdd3.contains(tuple) should be(true)
          }
        }
      }
    }
  }

  test("Intersection with table such that no more viable path") {
    val table1 = Array(Array(3,1,2),Array(3,2,1),Array(1,1,4),Array(1,2,1),Array(1,2,3))
    val table2 = Array(Array(1,1,1),Array(1,2,2),Array(2,2,2),Array(3,2,3))

    val mdd1 = StaticMdd.buildMddFromTableRegin(table1,3)
    val mdd2 = StaticMdd.buildMddFromTableRegin(table2,3)

    val mdd3 = StaticMdd.intersect(mdd1,mdd2)

    mdd3.numPaths().toString.toInt should equal(0)
  }


  test("Addition of tuples"){
    val table : Array[Array[Int]] = Array(
      Array(1,2,3,4),
      Array(1,2,4,3),
      Array(3,4,2,1),
      Array(2,3,4,1),
      Array(1,2,1,4),
      Array(4,4,4,4),
      Array(1,3,4,4),
      Array(2,2,2,2)
    )

    val mdd = StaticMdd.buildMddFromTableRegin(table,4)
    mdd.numPaths().toString should equal("8")
    mdd.contains(Array(1,2,4,3)) should be(true)
    mdd.contains(Array(1,2,3,4,5)) should be(false)
    mdd.contains(Array(1,2)) should be(false)

    mdd.addTuple(Array(1,2,3,4))
    mdd.numPaths().toString should equal("8")
    mdd.addTuple(Array(1,1,1,1))
    mdd.contains(Array(1,1,1,1)) should be(true)
    mdd.numPaths().toString should equal("9")

    mdd.removeTuple(Array(1,2,3))
    mdd.numPaths().toString should equal("9")
    mdd.removeTuple(Array(1,1,1,1,5))
    mdd.numPaths().toString should equal("9")
    mdd.contains(Array(1,1,1,1)) should be(true)

    mdd.removeTuple(Array(1,1,1,1))
    mdd.numPaths().toString should equal("8")
    mdd.contains(Array(1,1,1,1)) should be(false)
  }

  /**
    * Test of the regular constraint on an Mdd
    */
  test("Solutions of regular") {
    val nStates = 5
    val nLetters = 5
    val initState = 0
    val acceptingStates = new java.util.HashSet[Integer]()
    acceptingStates.add(3)
    acceptingStates.add(4)

    val automaton = new Automaton(nStates, nLetters, initState, acceptingStates)
    automaton.addTransition(0, 1, 0)
    automaton.addTransition(1, 2, 1)
    automaton.addTransition(2, 3, 3)
    automaton.addTransition(3, 4, 2)
    automaton.addTransition(2, 1, 1)
    automaton.addTransition(4, 0, 4)

    val sizes = Array(4, 5, 6, 8, 10, 11)
    for (size <- sizes) {
      val cp = new CPSolver()
      val cpMdd = new CPSolver()

      val x = Array.fill(size)(CPIntVar(0 to nLetters)(cp))
      val xMdd = Array.fill(size)(CPIntVar(0 to nLetters)(cpMdd))

      cp.search(binary(x))
      cpMdd.search(binary(xMdd))

      val mdd = StaticMdd.buildMddFromRegular(automaton, size)

      cp.add(new Regular(x, automaton))
      cpMdd.add(new Mdd4RConstraint(xMdd, mdd, cpMdd))

      val stats = cp.start()
      val statsMdd = cpMdd.start()

      stats.nSols should equal(statsMdd.nSols)
      stats.nFails should equal(statsMdd.nFails)
    }

  }

}
