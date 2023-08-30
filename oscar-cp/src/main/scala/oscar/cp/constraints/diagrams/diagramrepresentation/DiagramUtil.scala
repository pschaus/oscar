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

package oscar.cp.constraints.diagrams.diagramrepresentation

import oscar.cp.constraints.tables.tablerepresentation.{GroundTable, Table}
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset}

import scala.collection.mutable

/**
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */

abstract class Diagram {

  val nbLevel:Int

  def isEmpty:Boolean

  def nbNodes: Int

  def nbEdges: Int

  def nbEdges(level: Int): Int

  def nbSourceNodes(level: Int): Int

  def nbDestinationNodes(level: Int): Int

  /**
   * Check if the tuple is valid
   * @param x variables linked to the table
   * @param tuple tuple wanted
   * @return true if tuple is valid
   */
  def isTupleValid(x: Array[CPIntVar], tuple: Array[Int]): Boolean

  def getHashSet(x: Array[CPIntVar]): mutable.Set[String]

  def getHashGround(tuple: Array[Int]): String =
    tuple.mkString(",")

  def isEquivalentTo(x: Array[CPIntVar], groundTable: GroundTable): Boolean = {
    this.getHashSet(x) == groundTable.getHashSet(x)
  }

  /**
   * Filter the mdd regarding to a given variable
   * @param x variables linked to the table
   * @return new Mdd filtered
   */
  def filterDiagram(x: Array[CPIntVar]): Diagram

  /**
   * Create the offseted mdd. The offset is removed from each values
   * @param offsets the offsets for the variables
   * @return new Mdd offseted
   */
  def applyOffset(offsets: Array[Int]): Diagram


  /**
   * Method taking a Mdd and decompress it in a corresponding table (GroundMdd to GroundTable, BasicSmartMdd to BasicSmartTable)
   * @return corresponding table
   */
  def decompressToTable(x: Array[CPIntVar]): Table



  def compressToBasicSmartDiagram(x: Array[CPIntVar], withSets:Boolean = false): BasicSmartDiagram

  // reduce the tree (MDD case)
  def reduceTree_MDD: Diagram

  // reduce the tree (sMDD case)
  def reduceTree_sMDD: Diagram

  /**
   * Method allowing the filtering the table, create variables with views with domains starting at 0 and create the offseted table
   * @param x initial variable to associate the table to
   * @return array of shift view variable and offseted and filtered table
   */
  def normalizeMdd(x:Array[CPIntVar]):(Array[CPIntVar],Diagram) = {
    val offsets = Array.tabulate(x.length)(i => x(i).min)
    val viewsVars = Array.tabulate(x.length)(i => new CPIntVarViewOffset(x(i), -offsets(i)).asInstanceOf[CPIntVar])
    val newMdd = this.filterDiagram(x).applyOffset(offsets)
    (viewsVars, newMdd)
  }

}



