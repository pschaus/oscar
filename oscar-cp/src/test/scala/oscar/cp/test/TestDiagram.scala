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

import oscar.algo.Inconsistency
import oscar.cp.constraints.diagrams.diagramrepresentation.GroundDiagram
import oscar.cp.constraints.diagrams.{DiagramAlgo, diagram}
import oscar.cp.constraints.tables.TableDecomp
import oscar.cp.constraints.tables.tablerepresentation.GroundTable
import oscar.cp.testUtils.TestSuite
import oscar.cp.{CPIntVar, CPSolver, allDifferent, binaryFirstFailIdx}
import oscar.cp._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class TestDiagram extends TestSuite {

  private val rand = new scala.util.Random(16)

  private def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int) = {
    Array.fill(n, dim)(rand.nextInt(maxValue - minValue) + minValue)
  }

  // Create the unit tests
  for (i <- 1 to 100) {

    val tuples1_init = randomTuples(3, 100, 3, 8)
    val tuples2_init = randomTuples(3, 100, 2, 7)
    val tuples3_init = randomTuples(3, 100, 1, 6)

    val table1 = new GroundTable(tuples1_init).sortTable.removeDuplicate.asInstanceOf[GroundTable]
    val table2 = new GroundTable(tuples2_init).sortTable.removeDuplicate.asInstanceOf[GroundTable]
    val table3 = new GroundTable(tuples3_init).sortTable.removeDuplicate.asInstanceOf[GroundTable]



    val tuples1 = table1.getTable
    val tuples2 = table2.getTable
    val tuples3 = table3.getTable

    val mdd1 = table1.transformTo_MDD.asInstanceOf[GroundDiagram]
    val mdd2 = table2.transformTo_sMDD.asInstanceOf[GroundDiagram]
    val mdd3 = table3.transformTo_MDD.asInstanceOf[GroundDiagram]

    for (algo <- DiagramAlgo.values) {
      test("table : Test random diagram " + i + " (" + algo.toString + ")") {
        testDiagram(Array(tuples1, tuples2, tuples3), Array(mdd1, mdd2, mdd3), algo)
      }
    }
  }

  private def testDiagram(tables: Array[Array[Array[Int]]], diag: Array[GroundDiagram], algo: DiagramAlgo.Value): Unit = {

    implicit val solver = CPSolver()
    val x = Array.fill(5)(CPIntVar(1 to 8))

    val tup0 = diag(0).decompressToTable(x)
    val tup1 = diag(1).decompressToTable(x)
    val tup2 = diag(2).decompressToTable(x)
    assert(tup0.length == tables(0).length, "tab 0")
    assert(tup1.length == tables(1).length, "tab 1")
    assert(tup2.length == tables(2).length, "tab 2")

    solver.add(allDifferent(x))
    solver.search(binaryFirstFailIdx(x, i => x(i).max))

    val statRef = solver.startSubjectTo() {
      val cons = Seq(
        new TableDecomp(Array(x(0), x(1), x(2)), tables(0)),
        new TableDecomp(Array(x(2), x(3), x(4)), tables(1)),
        new TableDecomp(Array(x(0), x(2), x(4)), tables(2))
      )
      solver.add(cons)
    }
    val stat = solver.startSubjectTo() {
      val cons = Seq(
        diagram(Array(x(0), x(1), x(2)), diag(0), algo),
        diagram(Array(x(2), x(3), x(4)), diag(1), algo),
        diagram(Array(x(0), x(2), x(4)), diag(2), algo)
      )
      solver.add(cons)
    }

    if (stat.nSols != statRef.nSols) {
      println(algo + " " + stat.nSols + " " + statRef.nSols)
      tables(0).foreach(a => println(a.mkString(",")))
      println("")
    }

    assert(stat.nSols == statRef.nSols)
    assert(stat.nFails == statRef.nFails)

  }

  for (algo <- DiagramAlgo.values) {

    test("diagram : test 1 " + algo) {
      val cp = CPSolver()
      var x = Array.fill(3)(CPIntVar(1 to 3)(cp))

      val tuples = Array(Array(1, 1, 1), Array(1, 2, 3))
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]

      cp.post(diagram(Array(x(0), x(1), x(2)), diag, algo))

      x(0).isBound should be(true)
      x(0).value should be(1)
      x(2).hasValue(2) should be(false)

      cp.post(x(2) !== 3)

      cp.isFailed should be(false)
      x(1).value should be(1)
      x(2).value should be(1)
    }


    test("diagram : test 2 " + algo) {
      val cp = CPSolver()

      var x = CPIntVar(0 to 4)(cp)
      var y = CPIntVar(0 to 4)(cp)
      var z = CPIntVar(0 to 24)(cp)

      val tuples = (for (i <- 0 until 5; j <- i + 1 until 5) yield Array(i, j, i * 4 + j - 1)).toArray
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]
      cp.post(diagram(Array(x, y, z), diag, algo))
      cp.post(z === 0)
      x.value should be(0)
      y.value should be(1)
      z.value should be(0)

    }


    test("diagram : test 3 " + algo) {
      val cp = CPSolver()
      var x = Array.fill(3)(CPIntVar(1 to 7)(cp))
      val tuples = Array(Array(1, 1, 1), Array(1, 2, 3), Array(1, 2, 7), Array(2, 1, 4))
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]
      var nbSol = 0
      cp.add(diagram(x, diag, algo))
      cp.search {
        binaryStatic(x)
      } onSolution {
        nbSol += 1
      }
      cp.start
      nbSol should be(4)
    }

    test("diagram : test 4 " + algo) {
      val cp = CPSolver()
      var x = Array.fill(2)(CPIntVar(1 to 1)(cp))

      val tuples = Array(Array(1, 2), Array(2, 1))
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]

      intercept[Inconsistency] {
        cp.post(diagram(x, diag, algo))
      }
    }


    test("diagram : test5 " + algo) {

      def nbSol(newcons: Boolean) = {
        val cp = CPSolver()
        val x = Array.fill(4)(CPIntVar(Set(1, 3, 6, 9))(cp))

        val tuples = Array(Array(1, 2, 2, 4),
          Array(1, 2, 4, 8),
          Array(1, 1, 9, 6),
          Array(1, 1, 8, 6),
          Array(3, 1, 6, 9),
          Array(1, 9, 3, 1),
          Array(1, 9, 9, 9),
          Array(3, 6, 6, 6))


        val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]

        val cons = if (newcons) diagram(x, diag, algo) else new TableDecomp(x, tuples)
        cp.post(cons)
        var nbSol = 0
        cp.search {
          binaryFirstFail(x)
        } onSolution {
          nbSol += 1
        }
        cp.start
        nbSol
      }
      nbSol(false) should be(nbSol(true))

    }


    test("diagram : test 6 " + algo) {
      implicit val cp = CPSolver()
      var x = Array.fill(6)(CPIntVar(2 to 3)(cp))
      var nbSol = 0

      val tuples = Array(
        Array(2, 2, 3, 2, 2, 3),
        Array(2, 3, 2, 2, 3, 2)
      )
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]


      cp.post(diagram(x, diag, algo))
      cp.search(binaryStatic(x))
      cp.onSolution {
        //println(x.mkString(", "))
        nbSol += 1
      }
      cp.start()
      nbSol should be(2)

    }

    test("diagram : test 7 " + algo) {
      implicit val cp = CPSolver()
      var x = Array.fill(6)(CPIntVar(0 to 5)(cp))
      var nbSol = 0

      val tuples = Array(
        Array(5, 5, 0, 5, 1, 4),
        Array(5, 5, 0, 3, 1, 1),
        Array(3, 1, 4, 2, 3, 1)
      )
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]

      cp.post(diagram(x, diag, algo))
      cp.search(binaryStatic(x))
      cp.onSolution {
        //println(x.mkString(", "))
        nbSol += 1
      }
      cp.start()
      nbSol should be(3)
    }

    test("diagram : test 8 " + algo) {
      implicit val cp = CPSolver()
      var x = Array.fill(7)(CPIntVar(0 to 3)(cp))
      var nbSol = 0

      val tuples = Array(
        Array(1, 0, 3, 1, 0, 2, 1),
        Array(1, 3, 3, 3, 2, 1, 3),
        Array(1, 0, 3, 0, 0, 3, 1)
      )
      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]

      cp.post(diagram(x, diag, algo))
      cp.search(binaryStatic(x))
      cp.onSolution {
        //println(x.mkString(", "))
        nbSol += 1
      }
      cp.start()
      nbSol should be(3)
    }

    test("diagram : test 9 (becoming empty table) " + algo) {
      implicit val cp = CPSolver()
      val x = Array.fill(3)(CPIntVar(0 to 3)(cp))
      val tuples: Array[Array[Int]] = Array(Array(0, 1, 2))

      val diag = new GroundTable(tuples).transformTo_MDD.asInstanceOf[GroundDiagram]
      intercept[Inconsistency] {
        cp.post(x(0) !== 0)
        cp.post(diagram(x, diag, algo))
      }
    }
  }

  for (i <- 1 to 20) {
    test("diagram: test 10 mdd vs smdd "+ i) {
      implicit val cp = CPSolver()
      val x = Array.fill(10)(CPIntVar(0 to 10)(cp))
      val tuple = randomTuples(10,100,0,10)
      val table = new GroundTable(tuple).sortTable.removeDuplicate.asInstanceOf[GroundTable]
      val mdd = table.transformTo_MDD
      val smdd = table.transformTo_sMDD

      val mdd_tab = mdd.decompressToTable(x)
      val smdd_tab = smdd.decompressToTable(x)

      assert(table.length == mdd_tab.length)
      assert(table.length == smdd_tab.length)
      assert(mdd.isEquivalentTo(x,table))
      assert(smdd.isEquivalentTo(x,table))

    }
  }
}