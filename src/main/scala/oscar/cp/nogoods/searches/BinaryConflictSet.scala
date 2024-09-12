package oscar.cp.nogoods.searches

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.ReversibleInt
import oscar.cp.nogoods.decisions.Decision
import oscar.cp.nogoods.decisions.Remove
import oscar.cp.nogoods.decisions.Assign

/** @author Renaud Hartert ren.hartert@gmail.com */
class BinaryConflictSet(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Last successful assigned value for each variable
  private[this] val lastValues = Array.fill(nVariables)(Int.MinValue)

  // Current depth of the search tree
  private[this] val nAssignedRev = new ReversibleInt(store, 0)

  // Last conflict
  private[this] var maxAssigned: Int = -1
  private[this] var conflictAssign: Int = 0

  final override def reset(): Unit = maxAssigned = -1

  final override def nextDecision: Decision = {
    val nAssigned = updateAssigned()
    if (nAssigned >= nVariables) null
    else {

      // Trail the new depth
      nAssignedRev.value = nAssigned
      
      // Select the variable implied in the last conflict if any
      if (conflictAssign > nAssigned && !variables(order(conflictAssign)).isBound) {
        val deepestId = order(conflictAssign)
        // Insert the last conflict in the sequence
        System.arraycopy(order, nAssigned, order, nAssigned + 1, conflictAssign - nAssigned)
        order(nAssigned) = deepestId
      } 
      // Select the next variable suggested by the variable heuristic
      else if (nAssigned > maxAssigned) {
        maxAssigned = nAssigned
        val position = nextVariable(nAssigned)
        val varId = order(position)
        // Swap the next variable
        order(position) = order(nAssigned)
        order(nAssigned) = varId
      }

      conflictAssign = nAssigned
      val varId = order(nAssigned)
      val variable = variables(varId)
      val lastValue = lastValues(varId)
      val value = if (variable.hasValue(lastValue)) lastValue else valHeuristic(varId)
      
      // Decision
      new Assign(variable, value)
    }
  }
  

  @inline private def updateAssigned(): Int = {
    var d = nAssignedRev.value
    while (d < nVariables && variables(order(d)).isBound) {
      val varId = order(d)
      lastValues(varId) = variables(varId).min
      d += 1
    }
    d
  }

  @inline private def nextVariable(depth: Int): Int = {
    var minId = depth
    var min = Int.MaxValue
    var i = depth
    while (i < nVariables) {
      val varId = order(i)
      if (!variables(varId).isBound) {
        val m = varHeuristic(order(i))
        if (m < min) {
          min = m
          minId = i
        }
      }
      i += 1
    }
    minId
  }
}