package oscar.cp.nogoods.searches

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.ReversibleInt
import oscar.cp.nogoods.decisions.Decision
import oscar.cp.nogoods.decisions.LowerEq
import oscar.cp.nogoods.decisions.Greater

/** @author Renaud Hartert ren.hartert@gmail.com */
class RestartConflictSet(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store
  
  private[this] val assigned = Array.tabulate(nVariables)(i => i)
  private[this] val nAssignedRev = new ReversibleInt(store, 0)
  private[this] val nDecisionRev = new ReversibleInt(store, 0)
  private[this] var nAssigned = 0
  private[this] var maxAssigned = 0

  private[this] val priorities = new Array[Long](nVariables)
  private[this] var timestamp = 0L

  private[this] var conflictDecision: Int = -1
  private[this] var conflictVar: Int = -1
  
  final override def reset(): Unit = {
    conflictDecision = - 1
    conflictVar = -1
  }

  final override def nextDecision: Decision = {
    timestamp += 1
    updateAssigned()
    if (computeAllAssigned == nVariables) null
    else {   
      
      // Trail the number of assigned variables
      nAssignedRev.value = nAssigned
      
      // New decision level
      val decision = nDecisionRev.incr() - 1
      
      // Handle last conflict if any
      if (conflictDecision >= decision) priorities(conflictVar) = timestamp

      // Select the next variable and value
      val varId = nextVariable()
      val variable = variables(varId)
      val value = valHeuristic(varId)

      conflictDecision = decision
      conflictVar = varId

      // Alternatives
      new LowerEq(variable, value)
    }
  }

  @inline private def nextVariable(): Int = {
    if (nAssigned < maxAssigned) nextPriority
    else {
      maxAssigned = nAssigned
      nextHeuristic
    }
  }
  
  @inline private def nextPriority: Int = {
    var maxId = -1
    var max = -1L
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      val priority = priorities(varId)
      if (priority > max) {
        max = priority
        maxId = varId
      }
      i += 1
    }
    maxId
  }
  
  @inline private def nextHeuristic: Int = {
    var minId = -1
    var min = Int.MaxValue
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      val heuristic = varHeuristic(varId)
      if (heuristic < min) {
        min = heuristic
        minId = varId
      }
      i += 1
    }
    minId
  }
  
  @inline private def computeAllAssigned(): Int = {
    var i = nVariables
    var n = 0
    while (i > 0) {
      i -= 1
      if (variables(i).isBound) {
        n += 1
      }
    }
    n
  }
  
  @inline private def updateAssigned(): Unit = {
    nAssigned = nAssignedRev.value
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      if (variables(varId).isBound) {
        val tmp = assigned(nAssigned)
        assigned(nAssigned) = varId
        assigned(i) = tmp
        nAssigned += 1
      }
      i += 1
    }
  }
}