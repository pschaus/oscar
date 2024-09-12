package oscar.cp.scheduling.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.algo.SortUtils.mergeSort

import scala.annotation.tailrec
import scala.math.{max, min}
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * @author Steven Gay steven.gay@uclouvain.be
 */

// A propagator for the cumulative constraint
// Overload Checker using CumulativeThetaLambdaTree

final class OverloadChecker(
    starts:  Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
    heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "OverloadChecker") {
  priorityL2 = 2
  private val nTasks = starts.length
  
  // for mergeSort
  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val activeBySMin = Array.ofDim[Int](nTasks)
  private[this] val activeByEMax = Array.ofDim[Int](nTasks)
  
  private[this] val startEventTask     = Array.ofDim[Int](nTasks)
  private[this] val startEventEnvelope = Array.ofDim[Long](nTasks)
  private[this] val startEventWorkload = Array.ofDim[Long](nTasks)
  
  private[this] val endEventTask         = Array.ofDim[Int](nTasks)
  private[this] val endEventDate         = Array.ofDim[Int](nTasks)
  private[this] val startEventOfEndEvent = Array.ofDim[Int](nTasks)

  private[this] val sminEvent = Array.ofDim[Int](nTasks)  // task -> start event of smin(task)
  private[this] val emaxEvent = Array.ofDim[Int](nTasks)
  
  private[this] val tree = new CumulativeLambdaThetaTree(startEventEnvelope, startEventWorkload)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  override def propagate() = {
    updateCache()
    
    // removeExtremal()  // count on Time-Tabling to check that extremal tasks do not overload // wrong for OverloadChecking!
    
    
    // Prepare events for the theta tree
    val nActive = filterActiveSort(sortedBySMin, activeBySMin, smin)
                  filterActiveSort(sortedByEMax, activeByEMax, emax)

    var C: Long = capacity.max
    
    var pStartEvent, pEndEvent = 0
    
    @inline def addStartEvent(task: Int, date: Int, workload: Long, eventType: Array[Int]) = {
      startEventTask(pStartEvent) = task
      startEventWorkload(pStartEvent) = workload
      startEventEnvelope(pStartEvent) = C * date
      eventType(task) = pStartEvent
      pStartEvent += 1
    }
    
    @inline def addEndEvent(task: Int, date: Int, associatedEventType: Array[Int]) = {
      endEventTask(pEndEvent) = task
      endEventDate(pEndEvent) = date
      startEventOfEndEvent(pEndEvent) = associatedEventType(task)
      pEndEvent += 1
    }
    
    var sminp, emaxp = 0
    var energy = 0L  // mandatory energy
    var date = 0
    
    while (emaxp < nActive) {  // do all events until last interesting one = last emax
      // find next event date
      date = emax(activeByEMax(emaxp))
      if (sminp < nActive) date = min(date, smin(activeBySMin(sminp)))
      
      // process events 
      while (sminp < nActive && smin(activeBySMin(sminp)) == date) {
        val task = activeBySMin(sminp)
        addStartEvent(task, date, hmin(task) * dmin(task), sminEvent)
        sminp += 1
      }
      
      while (emaxp < nActive && emax(activeByEMax(emaxp)) == date) {
        val task = activeByEMax(emaxp)
        addEndEvent(task, date, sminEvent)
        emaxp += 1
      }
    }

    // events are ready, pass them to tree
    tree.reset(pStartEvent)
    
    // conventional overload checker loop, insert tasks by increasing endmax
    emaxp = 0
    while (emaxp < pEndEvent) {
      val task = endEventTask(emaxp)
      val sEvent = startEventOfEndEvent(emaxp)
      val maxEnvelope = C * endEventDate(emaxp)
      
      // extension to optional tasks: add optionals in lambda instead of theta 
      if (required(task)) {
        tree.addToTheta(sEvent)
        if (tree.thetaEnvelope > maxEnvelope) throw Inconsistency
      }
      else tree.addToLambda(sEvent)
      
      // remove all optionals that would make the resource overload
      while (tree.lambdaEnvelope > maxEnvelope) {
        // find task associated to overloading event and remove it from resource
        val optEvent = tree.getLambdaEvent()
        val optTask = startEventTask(optEvent)
        
        resources(optTask).removeValue(id)
        tree.remove(optEvent)
      }      
      
      emaxp += 1
    }
  }
}

object OverloadChecker {
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar],
            h: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id: Int) = {
    new OverloadChecker(s, d, e, h, r, capacity, id)
  }
}
