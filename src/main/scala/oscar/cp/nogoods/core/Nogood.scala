package oscar.cp.nogoods.core

import oscar.cp.nogoods.decisions.Decision
import oscar.cp.core.Constraint
import oscar.cp.or

/** @author Renaud Hartert ren.hartert@gmail.com */
class Nogood(val decisions: Array[Decision]) {
  
  def isEntailed: Boolean = decisions.forall(_.isTrue)
  
  def toConstraint: Constraint = {
    if (decisions.isEmpty) new Unfeasible(null)
    else or(decisions.map(_.toLiteral))
  }
  
  def size = decisions.length
  
  override def toString: String = decisions.mkString(", ")
}