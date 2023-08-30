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

package oscar.cp.constraints.tables

import oscar.algo.Inconsistency
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.constraints.tables.mdd.{MDDTableVar, ReversibleMDD}

/**
 * Implementation of the MDD-4R algorithm for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
class TableMDD4R (val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableMDD4R") {

  override def associatedVars(): Iterable[CPVar] = X

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1
  
  private[this] val arity = X.length
  if (table.length == 0)
    throw Inconsistency
  private[this] val mdd = new ReversibleMDD(X(0).store, table)
  
  /* Temporary arrays used during propagation */
  private[this] val setupArray = Array.fill(X.map(_.size).max)(0)
  private[this] val deltaArray = Array.fill(mdd.maxPossibleValues)(0)
  private[this] val edgesArray = Array.fill(mdd.maxPossibleNbEdges)(0)
  private[this] var deltaSize = 0
  private[this] var edgesSize = 0
  
  /* For each CPIntVar we associate a MDDTableVar to link the variable with the MDD */
  private[this] val sharedEdges = Array.fill(mdd.nbEdges)(-1)
  private[this] val vars = X.zipWithIndex.map { case (x, indexVar) => 
    new MDDTableVar(x.store, x, indexVar, mdd.nbOfValues(indexVar), mdd, mdd.nbOfEdgesByValue(indexVar), sharedEdges) 
  }

  /**
   * Set up the constraint :
   * 1) We ask to call propagate() whenever a variable domain has changed
   * 2) We fill the support for each value of each variable
   * 3) We remove the value that do not appear in the table/MDD
   * 4) We do a first propagation
   */
  override def setup(l: CPPropagStrength): Unit = {


    /* Put edges in valid set */
    for (edge <- 0 until mdd.nbEdges) {
      vars(mdd.varOfEdge(edge)).addEdge(mdd.valueOfEdge(edge), edge)
    }
    
    /* Remove values that do not belong to the MDD at all */
    vars.foreach(x => x.deleteValuesNotInMDD(setupArray) )
    
    /* Call propagate and track Delta */
    X.zipWithIndex.foreach { case (x, i) => 
//      x.filterWhenDomainChangesWithDelta(false)(delta => propagateDelta(vars(i), i, delta))
      x.callPropagateWhenDomainChanges(this)
    }

    propagate()
  }
  
  @inline override final def propagate(): Unit = {
    /* We check for each variable if one of them has changed since last propagate */
    var i = 0
    while (i < arity) {
      val tableVar = vars(i)
      if (tableVar.hasChanged) {
        deltaSize = tableVar.fillDeltaArray(deltaArray)
        update(tableVar, i)
      }
      i += 1
    }
  }
  
  /*@inline final def propagateDelta(tableVar: MDDTableVar, indexVar: Int, deltaVar: DeltaIntVar): Unit = {
    if (!tableVar.hasChanged) {
      return Suspend
    }
    
    var newDeltaSize = 0
    for (value <- deltaVar.values()) {
      val valueIndex = mdd.indexOfValue(indexVar, value)
      if (tableVar.hasValue(valueIndex)) {
        deltaArray(newDeltaSize) = valueIndex
        newDeltaSize += 1
      }
    }
    
    deltaSize = newDeltaSize
    update(tableVar, indexVar)
  }*/
  
  /**
   * We update the MDD by removing edges corresponding to the delta values. We
   * also delete edges (and remove values from the domain if necessary) that
   * became invalid during the process.
   */
  @inline final def update(tableVar: MDDTableVar, indexVar: Int): Unit = {
    
    var cptUp = 0
    var cptDown = 0
    var resetUp = false
    var resetDown = false
    
    var lvlUp = indexVar
    var lvlDown = indexVar + 1
    
    /* We first count how many edges we have to remove at this level, and
     * how many will remain. */
    var nbToRemove = tableVar.getNbOfEdgesToRemove(deltaSize, deltaArray)
    var nbToKeep = mdd.getNbOfEdgesLvl(indexVar) - nbToRemove
    mdd.setNbOfEdgesLvl(indexVar, nbToKeep)
    
    if (nbToRemove < nbToKeep) {
      /* We delete the edges normally */
      
      mdd.saveNodesSet(lvlDown)
      mdd.saveNodesSet(lvlUp)
      edgesSize = tableVar.getEdgesToRemove(deltaSize, deltaArray, edgesArray)
      
      var i = 0 
      while (i < edgesSize) {
        val edge = edgesArray(i)
        val a = mdd.resetDeleteEdgeUp(edge)
        val b = mdd.resetDeleteEdgeDown(edge)
        cptUp += a
        cptDown += b
        i += 1
      }
    }
    else {
      /* We reset the edges */
      resetUp = true
      resetDown = true
      
      edgesSize = tableVar.getEdgesToKeep(edgesArray)
      
      mdd.clearAndSaveNodesSet(lvlUp)
      mdd.clearAndSaveNodesSet(lvlDown)
      
      var i = 0
      while (i < edgesSize) {
        val edge = edgesArray(i)
        val a = mdd.resetRestoreEdgeUp(edge)
        val b = mdd.resetRestoreEdgeDown(edge)
        cptUp += a
        cptDown += b
        i += 1
      }
    }
    
    /* We delete the values from the active ones */
    tableVar.deleteDeltaValues(deltaSize, deltaArray)
    
    /* We go up in the MDD until we don't remove any other node and edge. */
    while (cptUp > 0) {
      lvlUp -= 1
      if (resetUp) {
        nbToRemove = mdd.getNbOfEdgesLvl(lvlUp) - cptUp
        nbToKeep = cptUp
      }
      else {
        nbToRemove = cptUp
        nbToKeep = mdd.getNbOfEdgesLvl(lvlUp) - cptUp
      }
      
      cptUp = 0
      mdd.setNbOfEdgesLvl(lvlUp, nbToKeep)
      
      if (nbToRemove < nbToKeep) {
        /* Normal deletion of the edges */
        mdd.saveNodesSet(lvlUp)
        resetUp = false
        edgesSize = mdd.resetGetEdgesToDeleteUp(lvlUp + 1, edgesArray)
        
        var i = 0
        while (i < edgesSize) {
          val edge = edgesArray(i)
          cptUp += mdd.resetDeleteEdgeUp(edge)
          /* We remove the edge in the support for the corresponding value */
          vars(lvlUp).removeEdgeForValue(edge, mdd.valueOfEdge(edge))
          i += 1
        }
      }
      else {
        /* Reset */
        resetUp = true
        mdd.clearAndSaveNodesSet(lvlUp)
        edgesSize = mdd.resetGetEdgesToKeepUp(lvlUp + 1, edgesArray)
        
        vars(lvlUp).clearSupports()
        
        var i = 0
        while (i < edgesSize) {
          val edge = edgesArray(i)
          cptUp += mdd.resetRestoreEdgeUp(edge)
          vars(lvlUp).restoreEdge(edge, mdd.valueOfEdge(edge))
          i += 1
        }
        
        vars(lvlUp).removeUnsupportedValues
      }
    }
    
    /* We go down in the MDD until we don't remove any other node and edge. */
    while (cptDown > 0) {
      if (resetDown) {
        nbToRemove = mdd.getNbOfEdgesLvl(lvlDown) - cptDown
        nbToKeep = cptDown
      }
      else {
        nbToRemove = cptDown
        nbToKeep = mdd.getNbOfEdgesLvl(lvlDown) - cptDown
      }
      
      cptDown = 0
      mdd.setNbOfEdgesLvl(lvlDown, nbToKeep)
      
      if (nbToRemove < nbToKeep) {
        /* Normal deletion of the edges */
        mdd.saveNodesSet(lvlDown + 1)
        resetDown = false
        edgesSize = mdd.resetGetEdgesToDeleteDown(lvlDown, edgesArray)
        
        var i = 0
        while (i < edgesSize){
          val edge = edgesArray(i)
          cptDown += mdd.resetDeleteEdgeDown(edge)
          
          vars(lvlDown).removeEdgeForValue(edge, mdd.valueOfEdge(edge))
          i += 1
        }
      }
      else {
        /* Reset of the edges */
        resetDown = true
        mdd.clearAndSaveNodesSet(lvlDown + 1)
        edgesSize = mdd.resetGetEdgesToKeepDown(lvlDown, edgesArray)
        
        vars(lvlDown).clearSupports()
        
        var i = 0
        while (i < edgesSize) {
          val edge = edgesArray(i)
          cptDown += mdd.resetRestoreEdgeDown(edge)
          vars(lvlDown).restoreEdge(edge, mdd.valueOfEdge(edge))
          i += 1
        }
        
        vars(lvlDown).removeUnsupportedValues
      }
      lvlDown += 1
    }
  }
  
}