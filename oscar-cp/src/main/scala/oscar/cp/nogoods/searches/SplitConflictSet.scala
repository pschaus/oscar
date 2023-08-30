package oscar.cp.nogoods.searches

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.ReversibleInt
import oscar.cp.nogoods.decisions.Decision
import oscar.cp.nogoods.decisions.LowerEq
import scala.util.Random

/** @author Renaud Hartert ren.hartert@gmail.com */
class SplitConflictSet(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Last successfuly assigned value for each variable
  private[this] val lastValues = Array.tabulate(nVariables) { i => Int.MinValue }

  // Current depth of the search tree
  private[this] val depthRev = new ReversibleInt(store, 0)

  // Maximum number of assigned variables
  private[this] var maxDepth: Int = -1

  // Depth in which the last conflict occured
  private[this] var conflictDepth: Int = -1

  private[this] var maxConflictDepth: Int = 0
  private[this] var minInsertDepth: Int = nVariables

  private[this] var restarted = false
  private[this] val rand = new Random(0)
  
  
  
  final override def reset(): Unit = {
    
    val array = rand.shuffle(order.toIndexedSeq).toArray
    System.arraycopy(array, 0, order, 0, nVariables)
    maxConflictDepth = 0
    conflictDepth = -1
  }
  
  final override def nextDecision: Decision = {
    val depth = updateAssigned()
    if (depth >= nVariables) null
    else {
      // Trail the depth of the search tree
      depthRev.value = depth

      // Adjust variables order according to the last conflict
      if (conflictDepth > depth && !variables(order(conflictDepth)).isBound) {
        // Assign the last conflicting variable first
        val varId = order(conflictDepth)
        System.arraycopy(order, depth, order, depth + 1, conflictDepth - depth)
        // Handle restart learning
        if (depth < minInsertDepth) minInsertDepth = depth
        if (conflictDepth > maxConflictDepth) maxConflictDepth = conflictDepth
        order(depth) = varId
        conflictDepth = -1
        //println(order.mkString(" "))
      } else if (depth > maxDepth) {
        // New depth level
        maxDepth = depth
        val position = nextVariable(depth)
        val varId = order(position)
        order(position) = order(depth)
        order(depth) = varId
      }

      // Variable and value
      conflictDepth = depth
      val varId = order(depth)
      val variable = variables(varId)
      val minValue = variable.min
      val maxValue = variable.max
      val lastValue = lastValues(varId)
      val value = /*if (minValue <= lastValue && lastValue <= maxValue) lastValue else*/ valHeuristic(varId)

      // Alternatives
      new LowerEq(variable, value)
    }
  }

  @inline private def updateAssigned(): Int = {
    var d = depthRev.value
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