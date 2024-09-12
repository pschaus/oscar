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

// An adaptation of Wolf & Schrader's Overload Checker (2005) to Vilim's TimeTableEdgeFinding rules (2011)

/*
 *  A checker for a cumultative resource.
 *  It expects to be associated with a time-tabling algorithm, 
 *  or else it does not check tasks that overload at extremities.
 *  
 *  This is an extension of the overload checker of Wolf & Schrader at INAP 2005.
 *  It adds the energy of mandatory parts, as in Time-Table Edge Finding of Vilim at CPAIOR 2011.
 *  This is as part of Ouellet & Quimper's TTEEF at CPAIOR 2013,
 *  except that it does not add mandatory energy in the same way.
 *  
 *  Instead of adding mandatory energy as tasks in the theta tree,
 *  we modify the envelope of start events and the checking of end events:
 *  we substract mandatory energy (m.e.) before smin(a) for smin events,
 *  and when checking emax(a), we add m.e. before emax(a) to the envelope given by the theta tree.
 *  This is also correct (although not obvious), and does not impact filtering strength.
 *  
 *  This scheme allows to insert only free tasks in the tree,
 *  instead of both free tasks and mandatory energy cut in fixed tasks.
 *  Knowing that the second solution might add 4n tasks to the tree,
 *  the envelope modification solution is much cheaper!
 */

final class TimeTableOverloadChecker(
    starts:  Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
    heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "TimeTableOverloadChecker") {
  priorityL2 = 2
  private val nTasks = starts.length
  
  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedBySMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val freeBySMin = Array.ofDim[Int](nTasks)
  private[this] val freeByEMax = Array.ofDim[Int](nTasks)
  private[this] val mandatoryBySMax = Array.ofDim[Int](nTasks)
  private[this] val mandatoryByEMin = Array.ofDim[Int](nTasks)
  
  private[this] val startEventTask     = Array.ofDim[Int](nTasks)
  private[this] val startEventEnvelope = Array.ofDim[Long](nTasks)
  private[this] val startEventWorkload = Array.ofDim[Long](nTasks)
  
  private[this] val endEventTask            = Array.ofDim[Int](nTasks)
  private[this] val endEventDate            = Array.ofDim[Int](nTasks)
  private[this] val endEventMandatoryEnergy = Array.ofDim[Long](nTasks)
  private[this] val startEventOfEndEvent    = Array.ofDim[Int](nTasks)

  private[this] val sminEvent = Array.ofDim[Int](nTasks)  // task -> start event of smin(task)
  private[this] val smaxEvent = Array.ofDim[Int](nTasks)  // task -> start event of smax(task)
  private[this] val eminEvent = Array.ofDim[Int](nTasks)  // ...
  private[this] val emaxEvent = Array.ofDim[Int](nTasks)
  
  private[this] val tree = new CumulativeLambdaThetaTree(startEventEnvelope, startEventWorkload)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  override def propagate() = {
    updateCache()
    
    removeExtremal()  // count on Time-Tabling to check that extremal tasks do not overload
    
    // Prepare events for the theta tree
    val nFree = filterFreeSort(sortedBySMin, freeBySMin, smin)
                filterFreeSort(sortedByEMax, freeByEMax, emax)
    val nMandatory = filterMandatorySort(sortedByEMin, mandatoryByEMin, emin)
                     filterMandatorySort(sortedBySMax, mandatoryBySMax, smax)
    
    var C: Long = capacity.max
    
    var pStartEvent, pEndEvent = 0
    
    @inline def addStartEvent(task: Int, date: Int, workload: Long, mandatoryEnergy: Long, eventType: Array[Int]) = {
      startEventTask(pStartEvent) = task
      startEventEnvelope(pStartEvent) = C * date - mandatoryEnergy
      startEventWorkload(pStartEvent) = workload
      eventType(task) = pStartEvent
      pStartEvent += 1
    }
    
    @inline def addEndEvent(task: Int, date: Int, mandatoryEnergy: Long, associatedEventType: Array[Int]) = {
      endEventTask(pEndEvent) = task
      endEventDate(pEndEvent) = date
      endEventMandatoryEnergy(pEndEvent) = mandatoryEnergy
      startEventOfEndEvent(pEndEvent) = associatedEventType(task)
      pEndEvent += 1
    }
    
    var p = 0

    var sminp, smaxp, eminp, emaxp = 0
    var energy = 0L  // mandatory energy
    var height = 0L
    var date = 0
    var prevDate = min(if (nFree > 0)      smin(freeBySMin(0)) else 0,
                       if (nMandatory > 0) smax(mandatoryBySMax(0)) else Int.MaxValue)
    
    while (emaxp < nFree) {  // do all events until last interesting one = last emax
      // find next event date
      date = emax(freeByEMax(emaxp))
      if (sminp < nFree)      date = min(date, smin(freeBySMin(sminp)))
      if (smaxp < nMandatory) date = min(date, smax(mandatoryBySMax(smaxp)))
      if (eminp < nMandatory) date = min(date, emin(mandatoryByEMin(eminp)))
      
      // update energy
      energy += height * (date - prevDate)
      prevDate = date
      
      // process events 
      while (sminp < nFree && smin(freeBySMin(sminp)) == date) {
        val task = freeBySMin(sminp)
        if (required(task))
          addStartEvent(task, date, hmin(task) * (dmin(task) - max(0, emin(task) - smax(task))), energy, sminEvent)
        else
          addStartEvent(task, date, hmin(task) * dmin(task), energy, sminEvent)
        sminp += 1
      }
      
      while (emaxp < nFree && emax(freeByEMax(emaxp)) == date) {
        val task = freeByEMax(emaxp)
        addEndEvent(task, date, energy, sminEvent)
        emaxp += 1
      }
      
      while (smaxp < nMandatory && smax(mandatoryBySMax(smaxp)) == date) {
        val task = mandatoryBySMax(smaxp)
        height += hmin(task)
        smaxp += 1
      }
      
      while (eminp < nMandatory && emin(mandatoryByEMin(eminp)) == date) {
        val task = mandatoryByEMin(eminp)
        height -= hmin(task)
        eminp += 1
      }
    }

    // events are ready, pass them to tree
    tree.reset(pStartEvent)
    
    // conventional overload checker loop, insert tasks by increasing endmax
    p = 0
    while (p < pEndEvent) {
      val task = endEventTask(p)
      val sEvent = startEventOfEndEvent(p)
      val maxEnvelope = C * endEventDate(p)
      
      // extension to optional task: add optionals in lambda instead of theta 
      if (required(task)) {
        tree.addToTheta(sEvent)
        if (tree.thetaEnvelope + endEventMandatoryEnergy(p) > maxEnvelope) throw Inconsistency
      }
      else tree.addToLambda(sEvent)
      
      // remove all optional that would make the resource overload
      while (tree.lambdaEnvelope + endEventMandatoryEnergy(p) > maxEnvelope) {
        // find task associated to overloading event and remove it from resource
        val optEvent = tree.getLambdaEvent()
        val optTask = startEventTask(optEvent)
        
        resources(optTask).removeValue(id)
        tree.remove(optEvent)
      }      
      
      p += 1
    }
  }
}


object TimeTableOverloadChecker {
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar],
            h: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id: Int) = {
    new TimeTableOverloadChecker(s, d, e, h, r, capacity, id)
  }
}
