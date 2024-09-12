package oscar.cp.nogoods.searches

import oscar.cp.core.variables.CPIntVar
import oscar.cp.nogoods.decisions.Decision
import oscar.cp.nogoods.decisions.Assign
import oscar.algo.search.Branching
import oscar.algo.search.Alternative

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class NogoodBranching extends Branching {
  // def reset(): Unit = Unit
  def nextDecision: Decision
  
  // should do nextDecision and return empty seq on null, seq of decision and opposite otherwise
  // better not be implemented to crash when run, for now
  def alternatives() : Seq[Alternative] = {
    val dec = nextDecision
    if (dec == null) Seq()
    else {
      Seq(() => { dec.apply() }, () => { dec.opposite.apply() })
    }
  }  
}

class StaticNogoodBranching(variables: Array[CPIntVar], valHeuristic: CPIntVar => Int) extends NogoodBranching {
  final override def nextDecision: Decision = {
    val first = variables.find(!_.isBound)
    if (first.isEmpty) null
    else {
      val variable = first.get
      new Assign(variable, valHeuristic(variable))
    }
  }
}