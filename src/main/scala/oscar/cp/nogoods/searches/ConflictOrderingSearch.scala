package oscar.cp.nogoods.searches

import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversibleInt
import oscar.cp.CPIntVar
import scala.collection.mutable.Queue
import oscar.cp.nogoods.decisions._

/*
 *  Conflict Ordering Search, basically orders the variables by latest conflict
 *  and assigns the latest conflicting one. 
 */

/**
 * @author Steven Gay aashcrahin@gmail.com
 */

object ConflictOrderingSearch {
  def apply(bra: NogoodBranching, doReset: Boolean = false)(implicit S: CPStore) = new ConflictOrderingSearch(bra, doReset)
}

class ConflictOrderingSearch(bra: NogoodBranching, doReset: Boolean = false)(implicit S: CPStore) extends NogoodBranching {  
  var lastVariables = List[CPIntVar]()
  
  var lastVariable: Option[CPIntVar] = None
  var lastDepth = 0
  val depth = new ReversibleInt(S, 0)
  
  override def reset() = {
    lastVariable = None
    lastDepth = 0
    if (doReset) lastVariables = List[CPIntVar]()
    bra.reset()
  }

  def nextDecision(): Decision = {
    val d = depth.incr
    
    // Step 1: if last conflicting variable is new, add in head position.
    if (d <= lastDepth) lastVariable foreach { x =>
      // move x to head if it is in lastVariables
      lastVariables = lastVariables filter (_ != x)      
      lastVariables = x +: lastVariables
      
      lastVariable = None
    }
    
    lastDepth = d
    
    // Step 2: if some variable in conflict set is not bound, branch on it
    lastVariables.foreach { x =>
      if (!x.isBound) {
        lastVariable = Some(x)
        return new LowerEq(x, x.min)
      }
    }
    
    // Step 3: if all conflict set variables are bound, ask heuristic
    
    val dec = bra.nextDecision
    
    if (dec == null) {
      lastVariable = None
      null
    }
    else {
      lastVariable = Some(dec.variable)
      dec
    }
  }
}
