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

package oscar.cp.constraints.tables.tablerepresentation

import oscar.cp.constraints.diagrams.diagramrepresentation.Diagram
import oscar.cp.constraints.tables.BasicSmartElement
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset}

import scala.collection.mutable

object TableUtil {

  def decompressToGroundTable(X:Array[CPIntVar], table: Array[Array[Int]], star: Int) = {
    val shortTable = new ShortTable(table, star, false)
    val groundTable = shortTable.decompressToGroundTable(X)
    groundTable.getTable
  }

  def decompressToGroundTable(X:Array[CPIntVar], table: Array[Array[BasicSmartElement]]) = {
    val shortTable = new BasicSmartTable(table, false)
    val groundTable = shortTable.decompressToGroundTable(X)
    groundTable.getTable
  }

  def removeDuplicate(table: Array[Array[Int]]) = {
    val groundTable = new GroundTable(table, false).sortTable.removeDuplicate.asInstanceOf[GroundTable]
    groundTable.getTable
  }
}


object Table {
  def apply(table: Array[Array[Int]], isSorted: Boolean) = new GroundTable(table, isSorted)

  def apply(table: Array[Array[Int]], star: Int, isSorted: Boolean) = new ShortTable(table, star, isSorted)

  def apply(table: Array[Array[BasicSmartElement]], isSorted: Boolean) = new BasicSmartTable(table, isSorted)

}

abstract class Table {

  /**
   * Compute the number of tuples entry in the table.
   *
   * @return number of tuple in the table.
   */
  def length: Int

  /**
   * Compute the arity of the table
   */
  def arity:Int

  /**
   * Compute the exact number of ground tuples corresponding to the table
   * Duplicate are counted only once.
   *
   * @param x variables linked to the table
   * @return number of ground tuple
   */
  def groundCount(x: Array[CPIntVar]): Int = this.getHashSet(x).size

  /**
   * Check if the ground tuple is included in one of the tuple of the table
   *
   * @param tuple tuple to test
   * @return true if a tuple of the table include it, false otherwise
   */
  def isTupleIncluded(tuple: Array[Int]): Boolean

  /**
   * Check if the tuple is valid regarding to the table and the domains
   *
   * @param x     variables linked to the table
   * @param tuple tuple wanted
   * @return true if tuple is valid
   */
  def isTupleValid(x: Array[CPIntVar], tuple: Array[Int]): Boolean = {
    isTupleIncluded(tuple) && isGroundTupleValid(x, tuple)
  }

  /**
   * Check if the tuple at tupleIndex is valid regarding to the domains
   *
   * @param x          variables linked to the table
   * @param tupleIndex index of the tuple wanted
   * @return true if tuple at tupleIndex is valid
   */
  protected def isTupleValid(x: Array[CPIntVar], tupleIndex: Int): Boolean

  /**
   * Check if the ground tuple is valid in accordance to the domains of the variables
   *
   * @param x           variables linked to the table
   * @param tuple       the tuple to test
   * @return true if tuple is valid in accordance to the domains
   */
  def isGroundTupleValid(x: Array[CPIntVar], tuple: Array[Int]): Boolean = {
    var i = x.length
    while (i > 0) {
      i -= 1
      if (!x(i).hasValue(tuple(i)))
        return false
    }
    true
  }

  /**
   * Get the hash of a given tuple from the table
   * @param x
   * @param index
   * @return
   */
  def getHashSetTuple(x: Array[CPIntVar], index: Int): mutable.Set[String]

  /**
   * Get the hash set of the table
   *
   * @param x the variables linked
   * @return the set of hash
   */
  def getHashSet(x: Array[CPIntVar]): mutable.Set[String]

  /**
   * Get the hash of a given ground tuple
   * @param tuple
   * @return
   */
  def getHashGround(tuple: Array[Int]): String =
    tuple.mkString(",")

  /**
   * Test if this table representation is equivalent to a given ground table
   * @param x
   * @param groundTable
   * @return
   */
  def isEquivalentTo(x: Array[CPIntVar], groundTable: GroundTable): Boolean = {
    this.getHashSet(x) == groundTable.getHashSet(x)
  }

  /**
   * Filter the table regarding to a given variable
   *
   * @param x variables linked to the table
   * @return new Table filtered
   */
  def filterTable(x: Array[CPIntVar]): Table

  /**
   * Filter the table by removing strictly equals tuples
   *
   * @return new Table filtered
   */
  def removeDuplicate: Table

  /**
   * Sort the table
   *
   * @return new Table sorted if table wasn't sorted, this if it was
   */
  def sortTable: Table

  /**
   * Create the offseted table. The offset is removed from each values
   *
   * @param offsets the offsets for the variables
   * @return new Table offseted
   */
  def applyOffset(offsets: Array[Int]): Table

  /**
   * Method taking a table and return the equivalent groundTable
   * Decompression of all the tuples
   * Don't remove duplicates
   *
   * @param x array of variables
   * @return new equivalent GroundTable
   */
  def decompressToGroundTable(x: Array[CPIntVar]): GroundTable

  /**
   * Method taking a table and return the equivalent groundTable
   * Decompression of all the tuples
   *
   * @param x array of variables
   * @return new equivalent GroundTable without duplicates
   */
  def decompressToGroundTableWithoutDuplicate(x: Array[CPIntVar]): GroundTable = {
    val table = this.decompressToGroundTable(x)
    table.removeDuplicate.asInstanceOf[GroundTable]
  }

  /**
   * Method taking a short table and return the equivalent table with basic smart element instead
   * No compression nor decompression is applied
   *
   * @param x array of variables
   * @return new equivalent BasicSmartTable
   */
  def mapToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable

  /**
   * Compress the table into a basicSmartTable
   * @param x the variables
   * @return the table
   */
  def compressToBasicSmartTable(x: Array[CPIntVar]): BasicSmartTable

  /**
   * Transform into an MDD
   * @return
   */
  def transformTo_MDD: Diagram

  /**
   * Transform into an sMDD
   * @return
   */
  def transformTo_sMDD: Diagram

  /**
   * Method allowing the filtering the table, create variables with views with domains starting at 0 and create the offseted table
   *
   * @param x initial variable to associate the table to
   * @return array of shift view variable and offseted and filtered table
   */
  def normalizeTable(x: Array[CPIntVar]): (Array[CPIntVar], Table) = {
    val offsets = Array.tabulate(x.length)(i => x(i).min)
    val viewsVars = Array.tabulate(x.length)(i => new CPIntVarViewOffset(x(i), -offsets(i)).asInstanceOf[CPIntVar])
    val newTable = this.filterTable(x).applyOffset(offsets)
    (viewsVars, newTable)
  }
}
