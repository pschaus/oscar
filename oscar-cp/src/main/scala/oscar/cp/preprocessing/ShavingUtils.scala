package oscar.cp.preprocessing

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.algo.search.DFSearch

/**
  * Sascha Van Cauwelaert
  * Renaud Hartert ren.hartert@gmail.com */
object ShavingUtils {

  /** Strenghten the lower bound of the objective by complete shaving on the forwardVariables */
  def strengthenLowerBound(problem: CPStore, forwardVariables: Array[CPIntVar], objective: CPIntVar): Unit = {
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalBound = objective.min
    var bestBound = Int.MaxValue
    search.onSolution {
      val bound = objective.min
      if (bound < bestBound) bestBound = bound
    }
    
    search.start(branching, _ => bestBound == originalBound) 
    
    // Update the bound 
    objective.updateMin(bestBound)
    problem.propagate()
  }
  
  /** Strenghten the upper bound of the objective by complete shaving on the forwardVariables */
  def strengthenUpperBound(problem: CPStore, forwardVariables: Array[CPIntVar], objective: CPIntVar): Unit = {
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalBound = objective.max
    var bestBound = Int.MinValue
    search.onSolution {
      val bound = objective.max
      if (bound > bestBound) bestBound = bound
    }
    
    search.start(branching, _ => bestBound == originalBound)
    
    // Update the bound
    objective.updateMax(bestBound)
    problem.propagate()
  }
  
  /** Reduce the domain of variables by complete shaving on the forwardVariables */
  def reduceDomains(problem: CPStore, forwardVariables: Array[CPIntVar], variables: Array[CPIntVar]): Unit = {
    
    val nVariables = variables.length
    val Variables = 0 until nVariables
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalSize = Array.tabulate(variables.length)(i => variables(i).size)
    val reducedDomains = Array.fill(variables.length)(Set[Int]())
    
    search.onSolution {
      var i = variables.length
      while (i > 0) {
        i -= 1
        for (value <- variables(i).iterator) reducedDomains(i) += value
      }
    }
    
    search.start(branching, _ => Variables.forall(i => reducedDomains(i).size == originalSize(i)))
    
    // Reduce the domains
    for (i <- Variables) {
      val domain = variables(i).iterator
      for (value <- domain) if (!reducedDomains(i).contains(value)) variables(i).removeValue(value)
    }

    problem.propagate()   
  }

  def minShavingDummy(problem: CPStore, variable: CPIntVar): Unit = {
    var fail = true
    var min = variable.min
    val max = variable.max
    while (fail && min <= max + 1) {
      problem.pushState()
      fail = isInconsistent(problem.post(variable === min))
      problem.pop()
      min += 1
    }
    variable.updateMin(min - 1)
  }

  /**
    * Performs a shaving on the min and max values of the variables.
    * Stops as soon as the min and max values of the domain of each variable does not trigger a Failure
    * If the domain of a variable becomes empty, it returns a Failure.
    *
    * @param problem The CPStore containing the variables
    * @param variables The variables on which the shaving is performed
    */
  def boundsShaving(problem: CPSolver, variables: Array[CPIntVar]): Unit = {
    var domainChange = true
    while (domainChange) {
      domainChange = minShaving(problem, variables) || maxShaving(problem, variables)
    }
  }

  /**
    * Assign a variable to a value and checks whether or not the assignment triggers a Failure
    *
    * @param problem The CPStore containing the variable
    * @param variable The variable to be assigned
    * @param value The value assigned to variable
    * @return A boolean that is true if assigning value to variable triggers a Failure and false otherwise
    */
  def assignFail(problem: CPStore, variable: CPIntVar, value: Int): Boolean = {
    problem.pushState()
    val fail = isInconsistent(problem.post(variable === value))
    problem.pop()
    fail
  }

  /**
    * Performs a shaving on the min values of the variables.
    * Stops as soon as the min value of the domain of each variable does not trigger a Failure
    * If the domain of a variable becomes empty, it returns a Failure.
    *
    * @param problem The CPStore containing the variables
    * @param variables The variables on which the shaving is performed
    * @return true if a domain has been modified
    */
  def minShaving(problem: CPStore, variables: Array[CPIntVar]): Boolean = {
    var i = 0
    var domainChange = false
    while (i < variables.length) {
      val variable = variables(i)
      val min = variable.min
      if (assignFail(problem, variable, min)) {
        var u = variable.max
        var l = min
        var m = 0
        var fail = false
        while (l != u) {
          m = l + (u - l) / 2
          problem.pushState()
          fail = isInconsistent(problem.post(variable <= m))
          problem.pop()
          if (fail) {
            l = m + 1
          }
          else {
            u = m
          }
        }
        if (l != min) {
          domainChange = true
          variable.updateMin(l)
        }
      }
      i += 1
    }
    domainChange
  }

  /**
    * Performs a shaving on the max values of the variables.
    * Stops as soon as the max value of the domain of each variable does not trigger a Failure
    * If the domain of a variable becomes empty, it returns a Failure.
    *
    * @param problem The CPStore containing the variables
    * @param variables The variables on which the shaving is performed
    * @return true if a domain has been modified
    */
  def maxShaving(problem: CPSolver, variables: Array[CPIntVar]): Boolean = {
    var i = 0
    var domainChange = false
    while (i < variables.length) {
      val variable = variables(i)
      val max = variable.max
      if (assignFail(problem, variable, max)) {
        var u = max
        var l = variable.min
        var m = 0
        var fail = false
        while (l != u) {
          m = l + (u - l) / 2
          problem.pushState()
          fail = isInconsistent(problem.post(variable > m))
          problem.pop()
          if (fail) {
            u = m
          }
          else {
            l = m + 1
          }
        }
        if (u != max) {
          domainChange = true
          problem.post(variable <= u)
        }
      }
      i += 1
    }
    domainChange
  }
}