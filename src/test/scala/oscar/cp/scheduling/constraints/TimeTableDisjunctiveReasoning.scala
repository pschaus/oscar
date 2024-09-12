package oscar.cp.scheduling.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import java.lang.Math._

import oscar.algo.Inconsistency
import oscar.algo.SortUtils._

import scala.collection.mutable.Set
import scala.collection.mutable.TreeSet

/*
 * For every pair of activities s.t. height(i) + height(j) > capacity,
 * enforce e(a) <= s(b) \/ e(b) <= s(a).
 * 
 * This algorithm considers some specific intervals to filter out pairs (a, b) such that
 * checking (a, b) can not lead to an update.
 * 
 * When durations are fixed,
 * a can only push b if dur(b) > smax(a) - emin(a)
 * We leave the case smax(a) < emin(a) to TT, so we don't need to consider such 'a's as pushers.
 * Have the activities sorted in increasing order of dur in sortedByDur,
 * and sorted in increasing order of smax - emin in sortedBySME.
 * Introduce a new pusher a, confront it with every pushee b that has dur(b) > SME(a)
 * 
 * To remove activities, take nonfixed activities,
 * remove all pushers that are of height
 * 
 *  
 */


/*
 *  Who can push and be pushed is important:
 *  
 */

final class TimeTableDisjunctiveReasoning(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "TimeTableDisjunctiveReasoning")
{
  priorityL2 = 3
  
  private[this] val nTasks = starts.length
  private[this] val dminF = Array.ofDim[Int](nTasks)
  private[this] val smaxF = Array.ofDim[Int](nTasks)
  private[this] val eminF = Array.ofDim[Int](nTasks)
    
  private[this] var dminFmax = 0
  private[this] var hminmax = 0

  private[this] val profile = new ProfileStructure(smin, smax, dmin, emin, emax, hmin, required, possible)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  @inline private def updateFreeAndMaxs(limit: Int) = {
    dminFmax = 0
    hminmax = 0
    
    var p = 0
    while (p < limit) {
      val a = activitiesToConsider(p)
      if (smax(a) < emin(a)) {
        smaxF(a) = emin(a)
        eminF(a) = smax(a)
        dminF(a) = dmin(a) - (emin(a) - smax(a))
      }
      else {
        smaxF(a) = smax(a)
        eminF(a) = emin(a)
        dminF(a) = dmin(a)
      }
      
      if (dminFmax < dminF(a)) dminFmax = dminF(a)
      if (hminmax < hmin(a)) hminmax = hmin(a)
      p += 1
    }
  }
  
  
  private[this] val pushers = Array.ofDim[Int](nTasks)
  private[this] val hPushers = Array.ofDim[Int](nTasks)  // height of the MOI, i.e. min of profile on MOI + hmin(a)
  
  
  @inline private def introducePushers(limit: Int, C: Int): Int = {
    var q = 0
    var p = 0
    
    val gapmin = C - hminmax

    while (p < limit) {
      val a = activitiesToConsider(p)
      // an activity can push with its free part if its duration is not 0
      // and some activity may not fit strictly inside its MOI
      if (required(a) && dminF(a) > 0 && smaxF(a) - eminF(a) < dminFmax) {
        // compute height at which a would push
        
        // extremities of moi
        hPushers(a) = min(profile.minInterval(a, eminF(a) - 1, eminF(a)),
                          profile.minInterval(a, smaxF(a), smaxF(a) + 1))

        // inside of moi, when a has no mandatory part and a can fit inside
        if (smax(a) >= emin(a) && smaxF(a) - eminF(a) >= dminF(a)) { 
          hPushers(a) = min(hPushers(a), profile.minHeightOf(a))
        }
        
        // height at which a pushes includes the height of a
        hPushers(a) += hmin(a)
          
        if (hPushers(a) > C) throw Inconsistency  // a has no support, TT should have taken care of it          
        
        // add only if some activity could be pushed using that height
        if (hPushers(a) > gapmin) {
          pushers(q) = a
          q += 1
        }
      }
      p += 1
    }

    q
  }
  


  
  private[this] var maxPushersEMinF = 0
  private[this] var minPushersSMaxF = 0
  
  @inline private def updatePushersTimeBounds(nPushers: Int) = {
    maxPushersEMinF = Int.MinValue
    minPushersSMaxF = Int.MaxValue
    
    var p = 0
    while (p < nPushers) {
      val a = pushers(p)
      if (eminF(a) > maxPushersEMinF) maxPushersEMinF = eminF(a)
      if (smaxF(a) < minPushersSMaxF) minPushersSMaxF = smaxF(a)
      p += 1
    }
  }
  
  @inline private def introducePushees(limit: Int): Int = {
    var q = 0
    var p = 0
    while (p < limit) {
      val b = activitiesToConsider(p)
      if (required(b)) {
        if ((smaxF(b) < maxPushersEMinF && emax(b) > minPushersSMaxF) || 
            (eminF(b) > minPushersSMaxF && smin(b) < maxPushersEMinF)) {
          pushees(q) = b
          q += 1
        }
      }
      p += 1
    }
    q
  }
  
  private[this] val pushees = Array.ofDim[Int](nTasks)
  
  final override def propagate(): Unit = {
    updateCache()
    val C = capacity.max
    
    // Step 0: trust TT to do checking, this propagator can not deduce anything on extremal fixed activities.
    if (C == capacity.min) removeExtremal()
    else removeImpossible()

    // TODO: building the profile is expensive, redo prefiltering to opt out when there are 0 pushers
    // This happens when moi of tasks are all larger than durations, i.e. often near the root.
    profile.rebuild(toConsider)    
    
    val limit = toConsider.limit.value
    
    // Step 1: we will not consider all n^2 pairs, we want to filter out some pairs where no update can happen
    // Step 1.1: get the maximum dmin and hmin of pushees
    updateFreeAndMaxs(limit)
    
    // Step 1.2: for every possible pusher, compute the size of the MOI and the height at which it pushes. 
    // introduce only those pushers that have allowingDMin < dminmax and allowingHeight < hminmax
    // This should especially filter out pushers of large MOI, which can not filter anything.
    val nPushers = introducePushers(limit, C)
    if (nPushers == 0)
      return

    // Step 1.3: get max emin and min smax of pushers
    updatePushersTimeBounds(nPushers)
    
    // Step 1.4: compare the time bounds of every possible pushee to extremal time bounds of pushers 
    // if a pushee would not get pushed by extremal time bound, do not introduce it. 
    val nPushees = introducePushees(limit)
    if (nPushees == 0)
      return
    
    // Step 2: confront every pushee to every pusher 
    var i = 0
    while (i < nPushers) {
      val a = pushers(i)
      val Ca = C - hPushers(a)
      
      var j = 0
      while (j < nPushees) {
        val b = pushees(j)
        
        if (hmin(b) > Ca && a != b) {  // a can push b
          if (eminF(a) > smaxF(b) && emax(b) > smaxF(a)) {
            ends(b).updateMax(smaxF(a))
          }
          
          if (eminF(b) > smaxF(a) && smin(b) < eminF(a)) {
            starts(b).updateMin(eminF(a))
          }
        }
        
        j += 1
      }
      i += 1
    }
  }
}


object TimeTableDisjunctiveReasoning {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) =
    new TimeTableDisjunctiveReasoning(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
}
