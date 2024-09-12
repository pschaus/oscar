package oscar.cp.scheduling.constraints

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.algo.SortUtils._

import scala.math.{max, min}
import scala.annotation.tailrec
import oscar.cp.scheduling.util.OpenSparseSet
import oscar.algo.array.ArrayHeapInt

/**
 * @author Steven Gay steven.gay@uclouvain.be
 */
/*
 *  Unary Detectable Precedences, with optional tasks and non-constant durations.
 *  As in "Extension of O(n log n) Filtering...", Vilim et al. Constraints 2005. 
 */

class UnaryDPSweep(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore)
extends Constraint(store, "UnaryDetectablePrecedences2") {
  val lr = new UnaryDPSweepLR(starts, durations, ends, resources, id) 
  val rl = new UnaryDPSweepLR(ends map(-_), durations, starts map(-_), resources, id)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ resources

  override def setup(strength: CPPropagStrength) = {
    store.add(Array(lr, rl))
  }
}




class UnaryDPSweepLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore)
extends UnaryTemplate(starts, durations, ends, resources, id, "UnaryDetectablePrecedencesLR")(store)
{
  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ resources

  private[this] val nTasks = starts.length
  
  priorityL2 = 3
  
  private def nextPowerOfTwo(k: Int): Int = {
    1 << math.ceil(math.log(k) / math.log(2)).toInt
  }
  
  // TODO: optimize the size of tree
  private[this] val workload    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks)) 
  private[this] val envelope    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // base envelope level
  private[this] val workloadOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum workload of this node using at most one optional
  private[this] val envelopeOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum envelope of an optional activity at this node

  private[this] val leafOfActivity  = Array.ofDim[Int](nTasks)  // activity -> leaf id
  
  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  // private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedBySMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val toConsiderBySMin = Array.ofDim[Int](nTasks)
  // private[this] val toConsiderByEMax = Array.ofDim[Int](nTasks)
  private[this] val toConsiderByEMin = Array.ofDim[Int](nTasks)
  private[this] val toConsiderBySMax = Array.ofDim[Int](nTasks)
  
  private [this] val heapByEMin = new ArrayHeapInt(nTasks)
  
  
  override def propagate(): Unit = {
    updateCache()
        
    // Step 1: Initialization
    // Step 1.1: sort activities by criteria of interest
    // TODO: we will only introduce task with smax < max{emin}, filter out the others
    // TODO: with variable durations, we may be more precise by adding the energy of i, dmin(i),
    //       not at smin(i), but at emin(i) - dmin(i)
    // TODO: we can skip the introduction of tasks with dmin(i) == 0 in the tree, however pruning these tasks is crucial
    

    // Step 1.2: initialize arrays of the tree
    /*
     *  Tree uses indices from 0 to 2 * nodes - 1
     *  Leaves go from nodes to 2 * nodes - 1
     *  Leaves are not all actually used, only those from nodes to nodes + nToConsider - 1  
     */
    val nToConsider = toConsider.limit.value
    val nodes = nextPowerOfTwo(nToConsider)
    var p = 0
    // Put tasks that have energy in a heap, by emin, for DP pruning events.
    // TODO: heapify can be done in linear time if the whole array is given to the right algorithm...

    filterSort(sortedBySMin, toConsiderBySMin, smin)
    // filterSort(sortedByEMax, toConsiderByEMax, emax)
    filterSort(sortedByEMin, toConsiderByEMin, emin)
    filterSort(sortedBySMax, toConsiderBySMax, smax)
    
    // make a map from activities to leaves
    p = nToConsider
    while (p > 0) {
      p -= 1
      val a = toConsiderBySMin(p)
      leafOfActivity(a) = nodes + p
    }

    // initialize nodes
    p = 2 * nodes
    while (p > 0) {
      p -= 1
      workload(p) = 0
      envelope(p) = Int.MinValue
      workloadOpt(p) = 0
      envelopeOpt(p) = Int.MinValue      
    }

    // Step 2: Do main loop
    p = 0
    var q = 0
    var nPruningEvents = nToConsider
    
    heapByEMin.clear()
    
    while (nPruningEvents > 0) {
      // find whether next event is in array or in heap of pushed back
      var eventDate = Int.MaxValue
      var i = -1
      
      if (p < nToConsider) {
        i = toConsiderByEMin(p)
        eventDate = emin(i)
      }
      
      if (!heapByEMin.isEmpty && heapByEMin.minKey <= eventDate) {  // the '<=' empties the heap first, then the queue
        i = heapByEMin.dequeue()
        eventDate = emin(i)
      }
      else p += 1      
      
      nPruningEvents -= 1
      
      // insert all consumption events      
      while (q < nToConsider && smax(toConsiderBySMax(q)) < eventDate) {
        val j = toConsiderBySMax(q)
        if (required(j)) addToTheta(j) else addToLambda(j)
        q += 1
      }
      
      // treat pruning event
      if (!required(i)) {
        if (envelope(1) > smax(i)) { // optional activity would be pushed too far
          resources(i).removeValue(id)
          toConsider.exclude(i)
          removeFromLambda(i)
        }
        else {
          val newEMin = envelope(1) + dmin(i)
          if (newEMin > emin(i)) { // push for later
            emin(i) = newEMin
            heapByEMin.enqueue(emin(i), i)
            nPruningEvents += 1
          }
          
        }
      }
      else {    // remove impossible optionals
        val mustRemoveI = workload(leafOfActivity(i)) > 0
        if (mustRemoveI) removeFromTheta(i)
      
        if (envelope(1) > smin(i)) {
          starts(i).updateMin(envelope(1))
          val newEMin = envelope(1) + dmin(i)
          if (newEMin > emin(i)) { // push event back
            // smin(i) = envelope(1) // not possible since theta lambda tree is fixed
            emin(i) = newEMin
            heapByEMin.enqueue(emin(i), i)
            nPruningEvents += 1
          }
        }
        
        while (envelopeOpt(1) > smax(i)) {
          val opt = getMaxOptional(1, nodes)  // get responsible optional leaf
          val b = toConsiderBySMin(opt-nodes)  // get activity of that leaf
          
          // remove b from resource
          resources(b).removeValue(id)
          toConsider.exclude(b)
          removeFromTheta(b)
        }
        
        if (mustRemoveI) addToTheta(i)
      }
    }
    removeExtremal()
  }
  
  @inline final def addToTheta(act: Int) = {
    val leaf = leafOfActivity(act)
    workload(leaf)    = dmin(act)
    envelope(leaf)    = smin(act) + dmin(act)
    workloadOpt(leaf) = dmin(act)
    envelopeOpt(leaf) = smin(act) + dmin(act)
    
    insertActivity(leaf / 2)
  }

  @inline final def addToLambda(act: Int) = {
    val leaf = leafOfActivity(act)
    // workload(leaf)    = 0
    // envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = dmin(act)
    envelopeOpt(leaf) = smin(act) + dmin(act)
    
    insertActivity(leaf / 2)
  }
  
  @inline final def removeFromTheta(act: Int) = {
    val leaf = leafOfActivity(act)
    workload(leaf)    = 0
    envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = 0
    envelopeOpt(leaf) = Int.MinValue
    
    insertActivity(leaf / 2)
  }

  @inline final def removeFromLambda(act: Int) = {
    val leaf = leafOfActivity(act)
    // workload(leaf)    = 0
    // envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = 0
    envelopeOpt(leaf) = Int.MinValue
    
    insertActivity(leaf / 2)
  }
  

  @tailrec
  final def insertActivity(t: Int): Unit = {
    if (t > 0) {
      val left = t << 1
      val right = 1 + left
        
      workload(t) = workload(left) + workload(right)
      envelope(t) = max(envelope(left) + workload(right), envelope(right))
      
      workloadOpt(t) = max(workloadOpt(left) + workload(right),
                           workload(left) + workloadOpt(right))
                           
      envelopeOpt(t) = max(envelopeOpt(right),
                         max(envelope(left) + workloadOpt(right),
                             envelopeOpt(left) + workload(right))
                        )
      
      // if (debug) println(s"envelope ${envelope(t)}, left = ${envelope(left)}, workload = ${workload(right)}, right = ${envelope(right)}")
      insertActivity(t/2)
    } 
  }
  
  /*
   * find which optional activity causes the value of envelopeOpt
   * This method is more lengthy than remembering the activity at each node,
   * but when activity removal is rare, recomputing on need is cheaper.
   */
  @tailrec
  final def getMaxOptional(t:Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val e = envelopeOpt(t) 
      
      if (e == envelopeOpt(right)) {
        getMaxOptional(right, tMax)
      }
      else if (e == envelopeOpt(left) + workload(right)) {
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
  
  private[this] val indices = new Array[Int](nTasks)
  // filters out tasks that should not be considered, then sorts them
  final def filterSort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Unit = {
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = byKey.length 
    var q = limit
    
    // extract only values to consider
    while (p > 0) {
      p -= 1
      val task = byKey(p)
      if (status(task) < limit) {
        q -= 1
        filtered(q) = task
        indices(q) = p
      }
    }
    
    // sort them
    mergeSort(filtered, keys, 0, limit, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = limit
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
  }

}

object UnaryDPSweep {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore) = 
    new UnaryDPSweep(starts, durations, ends, resources, id) 

}