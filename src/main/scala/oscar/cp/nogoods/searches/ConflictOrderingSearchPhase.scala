package oscar.cp.nogoods.searches

import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversibleInt
import oscar.cp.CPIntVar
import scala.collection.mutable.Queue
import oscar.cp.nogoods.decisions._
import scala.util.Random

/*
 *  Conflict Ordering Search, basically orders the variables by latest conflict
 *  and assigns the latest conflicting one. 
 */

/**
 * @author Steven Gay aashcrahin@gmail.com
 */
object ConflictOrderingSearchPhase {
  def apply(bra: NogoodBranching, resetProbability: Double = 0.0)(implicit S: CPStore) = new ConflictOrderingSearchPhase(bra, resetProbability)
}

class ConflictOrderingSearchPhase(bra: NogoodBranching, resetProbability: Double)(implicit S: CPStore) extends NogoodBranching {  
  var lastDecisions = List[Decision]()
  
  var lastDecision: Option[Decision] = None
  var lastDepth = 0
  val depth = new ReversibleInt(S, 0)

  
  override def reset() = {
    lastDecision = None
    lastDepth = 0
    if (Random.nextFloat() < resetProbability) lastDecisions = List[Decision]()
    else { // remove phase only
      lastDecisions = lastDecisions.map { dec =>
             if (dec.isInstanceOf[LowerEq]) new LowerEq(dec.variable, Int.MinValue)
        else if (dec.isInstanceOf[Assign])  new Assign(dec.variable, Int.MinValue)
        else dec
      }
    }
    
    bra.reset()
  }
  

  def nextDecision(): Decision = {
    val d = depth.incr
    
    // Step 1: if last conflicting variable is new, add in head position.
//    if (lastDecision.forall(!_.isEntailed)) lastDecision foreach { dec =>
    if (d <= lastDepth) lastDecision foreach { dec =>
      // move x to head if it is in lastDecisions
      lastDecisions = lastDecisions filter (_.variable != dec.variable)      
      lastDecisions = dec +: lastDecisions
      
      lastDecision = None
    }
    
    lastDepth = d
    
    // Step 2: if some last decision
    lastDecisions.foreach { dec =>
      val x = dec.variable  // if heuristic returns Failure, boom here 
        
      if (!x.isBound) {
        if (dec.isTrue || dec.opposite.isTrue)
          lastDecision = Some(new LowerEq(x, x.min))
        else lastDecision = Some(dec)
        
        return lastDecision.get
      }
    }
    
    // Step 3: if all conflict set variables are bound, ask heuristic
    val dec = bra.nextDecision
    
    if (dec == null) {
      lastDecision= None
      null
    }
    else {
      lastDecision = Some(dec)
      dec
    }

  }
}
