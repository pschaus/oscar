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

import oscar.cp.constraints.diagrams.diagramrepresentation.BasicSmartDiagram
import oscar.cp.constraints.diagrams.{BasicSmartDiagramAlgo, basicsmartdiagram}
import oscar.cp.constraints.tables._
import oscar.cp.constraints.tables.tablerepresentation.{BasicSmartTable, TableUtil}
import oscar.cp.testUtils.TestSuite
import oscar.cp.{CPIntVar, CPSolver, allDifferent, binaryFirstFailIdx}

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class TestBasicSmartDiagram extends TestSuite {

  private val rand = new scala.util.Random(16)

  private def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int, d: Int): Array[Array[BasicSmartElement]] = {
    Array.fill(n, dim) {
      val v = rand.nextInt(100)
      if (v < d)
        Star()
      else if (v < 2 * d)
        NotEqual(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 3 * d)
        LessEq(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 4 * d)
        GreatEq(rand.nextInt(maxValue - minValue) + minValue)
      else
        Equal(rand.nextInt(maxValue - minValue) + minValue)
    }
  }

  private def randomTuplesWithSet(dim: Int, n: Int, minValue: Int, maxValue: Int, d: Int): Array[Array[BasicSmartElement]] = {
    Array.fill(n, dim) {
      val v = rand.nextInt(100)
      if (v < d)
        Star()
      else if (v < 2 * d)
        NotEqual(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 3 * d)
        LessEq(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 4 * d)
        GreatEq(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 5 * d) {
        val set: Set[Int] = Set()
        for (value <- minValue to maxValue; if rand.nextInt(100) < 25)
          set + value
        println(set.mkString(","))
        InSet(set)
      }
      else if (v < 6 * d) {
        val set: Set[Int] = Set()
        for (value <- minValue to maxValue; if rand.nextInt(100) < 25)
          set + value
        NotInSet(set)
      }
      else
        Equal(rand.nextInt(maxValue - minValue) + minValue)
    }
  }

  // Create the unit tests
  for (i <- 1 to 100) {

    val tuples1_init = randomTuples(3, 100, 3, 8, 2)
    val tuples2_init = randomTuples(3, 100, 2, 7, 5)
    val tuples3_init = randomTuples(3, 100, 1, 6, 1)
    val tuples1set_init = randomTuplesWithSet(3, 100, 3, 8, 2)
    val tuples2set_init = randomTuplesWithSet(3, 100, 2, 7, 5)
    val tuples3set_init = randomTuplesWithSet(3, 100, 1, 6, 1)

    val table1 = new BasicSmartTable(tuples1_init).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]
    val table2 = new BasicSmartTable(tuples2_init).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]
    val table3 = new BasicSmartTable(tuples3_init).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]
    val table1set = new BasicSmartTable(tuples1set_init).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]
    val table2set = new BasicSmartTable(tuples2set_init).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]
    val table3set = new BasicSmartTable(tuples3set_init).sortTable.removeDuplicate.asInstanceOf[BasicSmartTable]

    val tuples1 = table1.getTable
    val tuples2 = table2.getTable
    val tuples3 = table3.getTable
    val tuples1set = table1set.getTable
    val tuples2set = table2set.getTable
    val tuples3set = table3set.getTable

    val mdd1 = table1.transformTo_MDD.asInstanceOf[BasicSmartDiagram]
    val mdd2 = table2.transformTo_sMDD.asInstanceOf[BasicSmartDiagram]
    val mdd3 = table3.transformTo_MDD.asInstanceOf[BasicSmartDiagram]
    val mdd1set = table1set.transformTo_MDD.asInstanceOf[BasicSmartDiagram]
    val mdd2set = table2set.transformTo_sMDD.asInstanceOf[BasicSmartDiagram]
    val mdd3set = table3set.transformTo_MDD.asInstanceOf[BasicSmartDiagram]

    for (algo <- BasicSmartDiagramAlgo.values) {
      test("basicSmartTable : Test random " + i + " (" + algo.toString + ")") {
        testDiagram(Array(tuples1_init, tuples2_init, tuples3_init),Array(mdd1,mdd2,mdd3), algo)
      }
      test("basicSmartTable : Test random with set " + i + " (" + algo.toString + ")") {
        testDiagram(Array(tuples1set_init, tuples2set_init, tuples3set_init),Array(mdd1set,mdd2set,mdd3set), algo)
      }
    }
  }

  private def testDiagram(tables: Array[Array[Array[BasicSmartElement]]], diag: Array[BasicSmartDiagram], algo: BasicSmartDiagramAlgo.Value): Unit = {

    implicit val solver = CPSolver()
    val x = Array.fill(5)(CPIntVar(1 to 8))

    solver.add(allDifferent(x))
    solver.search(binaryFirstFailIdx(x, i => x(i).max))

    val subX0 = Array(x(0), x(1), x(2))
    val subX1 = Array(x(2), x(3), x(4))
    val subX2 = Array(x(0), x(2), x(4))

    val statRef = solver.startSubjectTo() {
      val ground0 = TableUtil.decompressToGroundTable(subX0, tables(0))
      val ground1 = TableUtil.decompressToGroundTable(subX1, tables(1))
      val ground2 = TableUtil.decompressToGroundTable(subX2, tables(2))
      val cons = Seq(
        new TableDecomp(subX0, ground0),
        new TableDecomp(subX1, ground1),
        new TableDecomp(subX2, ground2)
      )
      solver.add(cons)
    }

    val stat = solver.startSubjectTo() {
      val cons = Seq(
        basicsmartdiagram(subX0, diag(0), algo),
        basicsmartdiagram(subX1, diag(1), algo),
        basicsmartdiagram(subX2, diag(2), algo)
      )
      solver.add(cons)
    }

    if (stat.nSols != statRef.nSols) {
      println(s"$algo ${stat.nSols} ${statRef.nSols}")
      tables(0).foreach(a => println(a.mkString(",")))
      println("")
    }

    assert(stat.nSols == statRef.nSols)
    assert(stat.nFails == statRef.nFails)
  }

}
