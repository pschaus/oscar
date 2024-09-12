package oscar.cp.scheduling.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.algo.SortUtils.mergeSort

import scala.annotation.tailrec
import java.lang.Math.max

import oscar.algo.Inconsistency
import oscar.cp.core.variables.{CPIntVar, CPVar}

/*
 *  Wolf & Schrader's overload checker, INAP 2005.
 *  Extended to take optional activities into account.
 *
 *  An overload interval is a task interval \Omega that has
 *  more workload (= sum h_i d_i) that it can hold (C * (lct_\Omega - est_\Omega)).
 *  If such an interval exists, then for any task a with lct_a <= lct_\Omega,
 *  either a is in \Omega, or est_a < est_\Omega ;
 *  in that last case, the energy envelope est_a
 *  
 *   
 *  This overload checker consists in starting with an empty balanced binary tree,
 *  iteratively inserting activity from a leaf and going up to the root,
 *  computing workload/envelopes.
 *  
 *  The leaves are the activities, sorted by est from left to right.
 *  The order in which they are inserted is by increasing lct.
 *  This allows the checking of all optimal intervals.
 *  
 *  Each node of the tree holds the workload of the leaves underneath it,
 *  workload(t) = workload(left(t)) + workload(right(t)).
 *     
 *  We encode a balanced tree as an array, heap-like.
 *  The root is at index 1, there is nothing at index 0.
 *  Left node of t is 2*t, right node is 2*t+1, parent is t/2.   
 *  Each field is in a different array.
 *  
 *  Instead of having activities in internal nodes,
 *  we only have them at leaves, this simplifies recursions.
 *  
 *  
 */

final class OverloadCheckerExtended(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
                         heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "OverloadCheckerExtended")
{  
  priorityL2 = 2
//  idempotent = true
  private[this] val nTasks = starts.length

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  private def nextPowerOfTwo(k: Int): Int = {
    1 << math.ceil(math.log(k) / math.log(2)).toInt
  }
  
  // TODO: floor(log_2(x)) can be computed in O(log(log(x))) by searching the right shift amount
  // or maybe math.log is fast enough
  // there are 2 * req nodes in the tree, node 0 is never used.
  // leaves are counted from bottom leftmost to right
  // TODO: optimize the size of tree
  private def leafRankToArray(rank: Int, nLeaves: Int): Int = {
    val r = nextPowerOfTwo(nLeaves)
    r + rank
  }
    
  // TODO: optimize the size of tree
  private[this] val workload    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks)) 
  private[this] val energy      = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // base energy level
  private[this] val workloadOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum workload of this node using at most one optional
  private[this] val energyOpt   = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum energy of an optional activity at this node
  

  private[this] val activitiesByLCT = Array.ofDim[Int](nTasks)  // required activities
  private[this] val leafActivity    = Array.ofDim[Int](nTasks)  // leaf -> activity
  private[this] val activityLeaf    = Array.ofDim[Int](nTasks)  // activity -> leaf
  private var C = capacity.max
  
  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  
  override def propagate(): Unit = {
    updateCache()
    C = capacity.max
    
    // Step 1: Initialize leaves
    mergeSort(sortedBySMin, smin, 0, nTasks, temp1, temp2) 
    mergeSort(sortedByEMax, emax, 0, nTasks, temp1, temp2)

    // Step 1.1: activities will be introduced by emax
    var q = 0
    var p = 0
    while (p < nTasks) {
      val a = sortedByEMax(p)
      if (possible(a) && hmin(a) > 0 && dmin(a) > 0) {
        activitiesByLCT(q) = a
        q += 1
      }
      p += 1
    }
    
    // Step 1.2: leaves are sorted by smin from left to right
    q = 0
    p = 0
    while (p < nTasks) {
      val a = sortedBySMin(p)
      if (possible(a) && hmin(a) > 0 && dmin(a) > 0) {
        leafActivity(q) = a
        q += 1
      }
      p += 1
    }

    
    // Step 1.3: once we can enumerate activities, we use leafActivity to enumerate leaves in the right order
    val nRequired = q
    p = 0
    while (p < nRequired) {
      val a = leafActivity(p)
      activityLeaf(a) = p
      p += 1
    }

    
    // Step 2: Do main loop
    // TODO: check leaves, even if it looks obvious!
    val rr = nextPowerOfTwo(nRequired)

    // Step 2.0: put every node value to 0
    p = 0
    val toReset = 2 * rr
    while (p < toReset) {
      workload(p) = 0
      energy(p) = Int.MinValue
      workloadOpt(p) = 0
      energyOpt(p) = Int.MinValue      
      p += 1
    }

    p = 0
    while (p < nRequired) { // insert activities into tree by increasing lct
      val a = activitiesByLCT(p)
      val l = activityLeaf(a)
      val t = l + rr
      
      if (required(a)) {
        workload(t)    = dmin(a) * hmin(a)
        energy(t)      = smin(a) * C + workload(t)
        workloadOpt(t) = dmin(a) * hmin(a)
        energyOpt(t)   = smin(a) * C + workloadOpt(t)

        insertActivity(t/2)
        
        // check overload
        if (energy(1) > C * emax(a)) {
          throw Inconsistency
        }
      }
      
      else {  // possible(a)
        workload(t)    = 0
        energy(t)      = Int.MinValue
        workloadOpt(t) = dmin(a) * hmin(a)
        energyOpt(t)   = smin(a) * C + workloadOpt(t)

        insertActivity(t/2)
      }
      
      // remove impossible optionals
      while (energyOpt(1) > C * emax(a)) {
        val opt = getMaxOptional(1, rr)
        
        val b = leafActivity(opt-rr)
        resources(b).removeValue(id)
        
        // remove b from tree
        workloadOpt(opt) = 0
        energyOpt(opt)      = Int.MinValue
        insertActivity(opt/2)
//        return Suspend
      }

      p += 1
    }
  }
  
  @tailrec
  final def insertActivity(t: Int): Unit = {
    if (t > 0) {
      val left = t << 1
      val right = 1 + left
        
      workload(t) = workload(left) + workload(right)
      energy(t) = max(energy(left) + workload(right), energy(right))
      
      workloadOpt(t) = max(workloadOpt(left) + workload(right),
                           workload(left) + workloadOpt(right))
                           
      energyOpt(t) = max(energyOpt(right),
                         max(energy(left) + workloadOpt(right),
                             energyOpt(left) + workload(right))
                        )
      
      insertActivity(t/2)
    } 
  }
  
  /*
   * find which optional activity causes the value of energyOpt
   * This method is more lengthy than remembering the activity at each node,
   * but when activity removal is rare, recomputing on need is cheaper.
   */
  @tailrec
  final def getMaxOptional(t:Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val e = energyOpt(t) 
      
      if (e == energyOpt(right)) {
        getMaxOptional(right, tMax)
      }
      else if (e == energyOpt(left) + workload(right)) {
        getMaxOptional(left, tMax)
      }
      else {
        getMaxOptionalWorkload(right, tMax)
      }
    }
  }
  
  
  /*
   * find which optional activity causes the value of workloadOpt
   */
  @tailrec
  final def getMaxOptionalWorkload(t: Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val w = workloadOpt(t)
      
      if (w == workloadOpt(left) + workload(right)) {
        getMaxOptionalWorkload(left, tMax)
      }
      else {
        assert(w == workload(left) + workloadOpt(right))
        getMaxOptionalWorkload(right, tMax)
      }
        
    }
  }

}


object OverloadCheckerExtended {
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar],
            h: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id: Int) = {
    new OverloadCheckerExtended(s, d, e, h, r, capacity, id)
  }
}
