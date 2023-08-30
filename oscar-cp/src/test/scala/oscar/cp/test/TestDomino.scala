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
import collection.immutable.SortedSet

class TestDomino extends TestSuite {

  test("Domino") {

    val nLines = 6
    val nCols = 7
    val Lines = 0 until nLines
    val Cols = 0 until nCols

    val values = Array(Array(2, 4, 6, 4, 5, 2, 5),
      Array(4, 1, 2, 1, 3, 2, 5),
      Array(4, 1, 6, 3, 3, 2, 6),
      Array(5, 6, 6, 1, 4, 3, 6),
      Array(3, 1, 5, 1, 1, 2, 3),
      Array(5, 6, 2, 3, 5, 4, 4))

    // gives a unique integer id to a domino, t = the two numbers on the domino				   
    def dominoId(t: (Int, Int)) = t._1.min(t._2) * 9 + t._1.max(t._2)
    // convert a coord entry to an integer index
    def toIndex(i: Int, j: Int) = i * nCols + j
    // check if i,j is a valid coord in values 
    def inBound(t: (Int, Int)) = t._1 >= 0 && t._1 < nLines && t._2 >= 0 && t._2 < nCols
    // for an entry (i,j) the neighbor entries
    def neighbors(i: Int, j: Int) = Set((i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)).filter(inBound(_))
    // for an entry (i,j) the neighbor indices
    def neighborIndices(i: Int, j: Int) = neighbors(i, j).map(t => toIndex(t._1, t._2))
    // for an entry (i,j) the neighbor values (up,down, left, right)
    def neighborValues(i: Int, j: Int) = neighbors(i, j).map(t => values(t._1)(t._2))

    val cp = CPSolver()

    // decision variables

    // for each entry (i,j), what is the other entry forming a domino with it
    val matchedNeighbor = Array.tabulate(nLines, nCols)((i, j) => CPIntVar(neighborIndices(i, j))(cp))

    // for each domino side (i,j) what is the id of it's domino in the solution
    val id = Array.tabulate(nLines, nCols)((i, j) => CPIntVar(neighborValues(i, j).map(dominoId(_, values(i)(j))))(cp))
    var nbSol = 0
    for (i <- Lines; j <- Cols) {
      val validTuples = for ((k, l) <- neighbors(i, j)) yield (toIndex(k, l), dominoId(values(i)(j), values(k)(l)))
      // makes the link between the matchedNeighbor and the id of the domino
      cp.add(table(matchedNeighbor(i)(j), id(i)(j), validTuples))
      //  an entry is matched to another neighbor entry iff the other is also matched with it (reciprocity)
      //  This translates as x[i] == j <=> x[j] == i or with element constraint x[x[i]] = i		 
      cp.add(elementVar(matchedNeighbor.flatten, matchedNeighbor(i)(j), toIndex(i, j)), Strong)
    }
    // each domino can appear at most once so each domino id can appear at most twice
    cp.add(gcc(id.flatten, 0 to 9 * 9, 0, 2))

    cp.search {
      binaryFirstFail(id.flatten.toSeq)
    }

    cp.start().nSols should be(1)

  }

}
