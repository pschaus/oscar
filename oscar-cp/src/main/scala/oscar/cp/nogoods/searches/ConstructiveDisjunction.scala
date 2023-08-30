package oscar.cp.nogoods.searches

import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPStore
import oscar.cp.nogoods.decisions.Decision

/** @author Renaud Hartert ren.hartert@gmail.com */
object ConstructiveDisjunction {

  def reduceDomains(problem: CPStore, variables: Array[CPIntVar], decision: Decision): Unit = {

    val nVariables = variables.length
    val mins = Array.tabulate(nVariables)(i => variables(i).min)
    val maxs = Array.tabulate(nVariables)(i => variables(i).max)
    val filteredMins = Array.fill(nVariables)(0)
    val filteredMaxs = Array.fill(nVariables)(0)
    val boolean = decision.toLiteral
    
    var nReduced = 0

    // Apply first decision
    problem.pushState()
    problem.post(boolean === 1)

    if (problem.isFailed) {
      problem.pop()
      problem.post(boolean === 0)
    } else {
      // Save domain state
      var i = 0
      while (i < nVariables) {
        filteredMins(i) = variables(i).min
        filteredMaxs(i) = variables(i).max
        i += 1
      }
      problem.pop()
      // Apply second decision
      problem.pushState()
      problem.post(boolean === 0)
      if (problem.isFailed) {
        problem.pop()
        problem.post(boolean === 1)
      } else {
        var i = 0
        while (i < nVariables) {
          filteredMins(i) = math.min(variables(i).min, filteredMins(i))
          filteredMaxs(i) = math.max(variables(i).max, filteredMaxs(i))
          if (filteredMins(i) > mins(i) || filteredMaxs(i) < maxs(i)) nReduced += 1
          i += 1
        }
        problem.pop()
        // Apply shaving
        i = 0
        while (i < nVariables) {
          variables(i).updateMin(filteredMins(i))
          variables(i).updateMax(filteredMaxs(i))
          i += 1
        }        
      }
    }

    //println("nReduced: " + nReduced)
    problem.propagate()
  }
}