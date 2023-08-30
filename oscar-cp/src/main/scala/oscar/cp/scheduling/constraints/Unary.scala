package oscar.cp.scheduling.constraints

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.algo.SortUtils._

import scala.math.{max, min}
import scala.annotation.tailrec
import oscar.cp.scheduling.util.OpenSparseSet
import oscar.algo.reversible.ReversibleInt

/**
 * @author Steven Gay steven.gay@uclouvain.be
 */
/*
 *  Unary Detectable Precedences, with optional tasks and non-constant durations.
 *  As in "Extension of O(n log n) Filtering...", Vilim et al. Constraints 2005. 
 *  Added NFNL, since it fill the theta tree in the same order.
 */
class Unary(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)
extends Constraint(starts(0).store, "Unary") {
  val lr = new UnaryLR(starts, durations, ends, resources, id) 
  val rl = new UnaryLR(ends map(-_), durations, starts map(-_), resources, id)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ resources

  override def setup(strength: CPPropagStrength) = {
    s.add(Array(lr, rl))
  }
}




class UnaryLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)
extends UnaryTemplate(starts, durations, ends, resources, id, "UnaryLR")(starts(0).store)
{
  private[this] val nTasks = starts.length

  // private final val compareFlag = true
  priorityL2 = 3

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ resources

  private def nextPowerOfTwo(k: Int): Int = {
    1 << math.ceil(math.log(k) / math.log(2)).toInt
  }
  
  private[this] val startEventEnvelope = Array.ofDim[Long](nTasks)
  private[this] val startEventWorkload = Array.ofDim[Long](nTasks)
  private[this] val taskToStartEvent   = Array.ofDim[Int](nTasks)
  
  private[this] val tree = new CumulativeLambdaThetaTree(startEventEnvelope, startEventWorkload)
  
  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin, sortedByEMax, sortedByEMin, sortedBySMax = Array.tabulate(nTasks){ i => i }
  private[this] val bySMinMax, bySMaxMax, byEMinMax, byEMaxMax = new ReversibleInt(s, nTasks)
  
  private[this] val toConsiderBySMin, toConsiderByEMax, toConsiderByEMin, toConsiderBySMax = Array.ofDim[Int](nTasks)
  
  private[this] val indices = new Array[Int](nTasks)
  
  // filters out tasks that should not be considered, then sorts them
  final def filterSort(byKey: Array[Int], maxIndex: ReversibleInt, filtered: Array[Int], keys: Array[Int]): Unit = {
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    if (limit == 0) return ()

    // find last index where there is a value to consider
    var pMax = maxIndex.value
    val oldPMax = pMax
    var continue = pMax > 0
    
    pMax -= 1
    while (continue) {
      val task = byKey(pMax)
      if (status(task) < limit) continue = false
      else pMax -= 1
    }
    pMax += 1
    if (pMax < oldPMax) maxIndex.setValue(pMax)

    var p = pMax 
    
    //var p = nTasks
    var q = limit
    
    // extract only values to consider
    while (q > 0) {
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
  
  override def propagate(): Unit = {
    updateCache()
    
    filterSort(sortedBySMin, bySMinMax, toConsiderBySMin, smin)
    filterSort(sortedByEMax, byEMaxMax, toConsiderByEMax, emax)
    filterSort(sortedByEMin, byEMinMax, toConsiderByEMin, emin)
    filterSort(sortedBySMax, bySMaxMax, toConsiderBySMax, smax)
    
    // Step 1.1: prepare lambda theta tree events
    val nToConsider = toConsider.limit.value
    
    var p = 0
    while (p < nToConsider) {
      val task = toConsiderBySMin(p)
      taskToStartEvent(task) = p
      
      startEventEnvelope(p) = smin(task) 
      startEventWorkload(p) = dmin(task)
      p += 1
    }
    
    tree.reset(nToConsider)

    // Step 2: Do main loop
    var pEMin = 0
    var pEMax = 0
    
    var pEnergyEvents = 0
    var latestThetaTask = -1
    
    while (pEMax < nToConsider) {
      // find next event, either emin or emax
      var i = toConsiderByEMax(pEMax)
      var pruningEventDate = emax(i)
      var pruningEventIsEmax = true
      
      if (pEMin < nToConsider && emin(toConsiderByEMin(pEMin)) <= pruningEventDate) {
        i = toConsiderByEMin(pEMin)
        pruningEventDate   = emin(i)
        pruningEventIsEmax = false
        pEMin += 1
      }
      else pEMax += 1
      
      // insert tasks that must be before pruning event
      while (pEnergyEvents < nToConsider && smax(toConsiderBySMax(pEnergyEvents)) < pruningEventDate) {
        val j = toConsiderBySMax(pEnergyEvents)
        
        if (required(j)) {
          if (tree.thetaEnvelope > smax(j))
              ends(j).updateMax(smax(latestThetaTask))
          latestThetaTask = j
          tree.addToTheta(taskToStartEvent(j))
          // println(s"Added $j at ${smin(j)} == ${startEventEnvelope(taskToStartEvent(j))}, with workload ${dmin(j)} == ${startEventWorkload(taskToStartEvent(j))}, new envelope is ${tree.thetaEnvelope}")
        }
        else {
          if (tree.thetaEnvelope > smax(j) && emin(j) > smax(latestThetaTask)) {
            resources(j).removeValue(id)
            toConsider.exclude(j)
          }
          else tree.addToLambda(taskToStartEvent(j))
        }

        pEnergyEvents += 1
      }

      
     if ( (pruningEventIsEmax && tree.thetaEnvelope > smax(i)) ||
         (!pruningEventIsEmax && tree.thetaEnvelope > smin(i))
         ) {
          if (required(i)) { // remove impossible optionals
            val mustRemoveI = tree.isEventInTheta(taskToStartEvent(i))
            if (mustRemoveI) tree.remove(taskToStartEvent(i))
            
            if (pruningEventIsEmax) {
              if (tree.thetaEnvelope > smax(i)) {
                // println(s"mustRemoveI = $mustRemoveI smax($i) = ${smax(i)}, envelope is ${tree.thetaEnvelope}")
                val newEMax = smax(latestThetaTask)
                if (newEMax < emax(i))
                  ends(i).updateMax(newEMax)
              } 
            }
            else {
              val newSMin = tree.thetaEnvelope  // do not convert toInt here: if envelope is Long.MinValue, boom
              // println(s"mustRemoveI = $mustRemoveI smin($i) = ${smin(i)}, envelope is ${newSMin} == ${tree.thetaEnvelope}")
              if (newSMin > smin(i))
                starts(i).updateMin(newSMin.toInt)
              
              // remove optionals
              while (tree.lambdaEnvelope > smax(i)) {
                val opt = tree.getLambdaEvent()
                
                val b = toConsiderBySMin(opt)
                resources(b).removeValue(id)
                toConsider.exclude(b)
                
                tree.remove(opt)
              }
            }
            
            if (mustRemoveI) tree.addToTheta(taskToStartEvent(i))
          }
          else {
            if (tree.thetaEnvelope > smax(i)) {
              if ((pruningEventIsEmax && smax(latestThetaTask) < emin(i)) || !pruningEventIsEmax) {
                resources(i).removeValue(id)
                // println((if (emaxEvent) "NFNL" else "DP") + " removing with a push")
                toConsider.exclude(i)
                tree.remove(taskToStartEvent(i))
              }
            }
          }
      }
    }
    
    
    // Edge-Finding + Overload Checking
     while (pEMax > 0) {
        pEMax -= 1
        
        val j = toConsiderByEMax(pEMax)
        
        if (required(j) && tree.thetaEnvelope > emax(j)) throw Inconsistency  // overload
        
        // remove optional with overload checking
        while (tree.lambdaEnvelope > emax(j)) {
          val opt = tree.getLambdaEvent()
          val i = toConsiderBySMin(opt)
          
          if (required(i)) { // apply Edge Finding
            if (tree.thetaEnvelope > smin(i))
              starts(i).updateMin(tree.thetaEnvelope.toInt)
            tree.remove(taskToStartEvent(i))
          }
          else {  // remove responsible, using Overload Checking
            resources(i).removeValue(id)
            toConsider.exclude(i)
            tree.remove(taskToStartEvent(i))
          }
        }
        
        if (required(j)) {
          // move from theta to lambda
          tree.addToLambda(taskToStartEvent(j))
        }
        else {
          tree.remove(taskToStartEvent(j))
        }
        
      }
       
    
    removeExtremal()
    //removeIsolated(toConsiderBySMin, toConsiderByEMax, nToConsider)
  }
}


object Unary {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int) =
    new Unary(starts, durations, ends, resources, id) 
}
