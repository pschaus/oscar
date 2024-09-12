package oscar.cp.nogoods.searches

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.ReversibleInt
import oscar.cp.nogoods.decisions.LowerEq
import oscar.cp.nogoods.decisions.Decision
import oscar.algo.reversible.ReversibleBoolean

/** @author Renaud Hartert ren.hartert@gmail.com */
class BadDecisionSearch(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store

  private[this] val allDecisions = Array.tabulate(nVariables)(varId => {
    Array.tabulate(variables(varId).max + 1)(v => new LowerEq(variables(varId), v))
  })

  private[this] val allPriorities = Array.tabulate(nVariables)(varId => {
    Array.fill(variables(varId).max + 1, 2)(0L)
  })

  private[this] val allApplied = Array.tabulate(nVariables)(varId => {
    Array.fill(variables(varId).max + 1, 2)(new ReversibleBoolean(store, false))
  })

  private[this] val assigned = Array.tabulate(nVariables)(i => i)
  private[this] val nAssignedRev = new ReversibleInt(store, 0)
  private[this] val levelRev = new ReversibleInt(store, 0)
  private[this] var nAssigned = 0
  private[this] var maxAssigned = 0

  private[this] var timestamp = 0L
  private[this] var conflictLevel: Int = -1
  private[this] var conflictVar: Int = -1
  private[this] var conflictVal: Int = -1
  private[this] var conflictSign: Int = 0

  final override def reset(): Unit = {
    conflictLevel = -1
  }

  final override def nextDecision: Decision = {
    timestamp += 1
    updateAssigned()
    if (nAssigned == nVariables) null
    else {

      // Trail the number of assigned variables
      nAssignedRev.value = nAssigned

      // New decision level
      val level = levelRev.incr() - 1

      // Handle last conflict if any
      if (conflictLevel >= level) {
        allPriorities(conflictVar)(conflictVal)(conflictSign) = timestamp
      }

      // Select the next variable and value
      val (varId, value, sign) = next()
      conflictLevel = level
      conflictVar = varId
      conflictVal = value
      conflictSign = sign

      allApplied(varId)(value)(sign).setTrue()

      // Alternatives
      if (sign == 1) allDecisions(varId)(value)
      else allDecisions(varId)(value).opposite
    }
  }

  @inline private def nextPriority(): (Int, Int, Int) = {
    var maxVar = -1
    var maxVal = -1
    var maxSign = -1
    var maxPriority = -1L
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      val variable = variables(varId)
      val priorities = allPriorities(varId)
      val minValue = variable.min
      val maxValue = variable.max
      var value = minValue
      while (value <= maxValue) {
        val alreadyApplied = allApplied(varId)(value)(0).value || allApplied(varId)(value)(1).value
        val priority0 = priorities(value)(0)
        val priority1 = priorities(value)(1)
        val priority = priority0 + priority1
        if (priority > maxPriority && !alreadyApplied) {
          maxPriority = priority
          maxVar = varId
          maxVal = value
          maxSign = if (priority0 > priority1) 0 else 1
        }
        value += 1
      }
      i += 1
    }
    (maxVar, maxVal, maxSign)
  }

  @inline private def next(): (Int, Int, Int) = {
    if (nAssigned < maxAssigned) {
      nextPriority()
    } else {
      maxAssigned = nAssigned
      nextHeuristic()
    }
  }

  @inline private def nextHeuristic(): (Int, Int, Int) = {
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
    val value = valHeuristic(minId)
    val sign = 1
    (minId, value, sign)
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