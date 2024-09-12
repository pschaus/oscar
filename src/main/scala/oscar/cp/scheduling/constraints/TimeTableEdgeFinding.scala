package oscar.cp.scheduling.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling._
import java.lang.Math._

import oscar.algo.Inconsistency
import oscar.algo.SortUtils._
import oscar.cp.core.variables.CPVar

/*
 * TTEF of Vilim 2011 as described in Schutt et al 2013 
 */

class TimeTableEdgeFinding(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends Constraint(capacity.store, "TimeTableEdgeFinding") {
  private val lr = new TimeTableEdgeFindingLR(starts, durations, ends, heights, resources, capacity, id) 
  private val rl = new TimeTableEdgeFindingLR(ends map(-_), durations, starts map(-_), heights, resources, capacity, id)
  private val store = capacity.store

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  override def setup(strength: CPPropagStrength) = {
    store.post(Array[Constraint](lr, rl))
  }
}


final class TimeTableEdgeFindingLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "TimeTableEdgeFinding")
{
  priorityL2 = 1
  private[this] val nTasks = starts.length

  private[this] var C = 0L  // resource capacity

  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedBySMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val activeBySMin = Array.ofDim[Int](nTasks)
  private[this] val activeByEMax = Array.ofDim[Int](nTasks)
  private[this] val mandatoryBySMax = Array.ofDim[Int](nTasks)
  private[this] val mandatoryByEMin = Array.ofDim[Int](nTasks)
  
  private[this] val smaxF = Array.ofDim[Int](nTasks)
  private[this] val eminF = Array.ofDim[Int](nTasks)
  private[this] val dminF = Array.ofDim[Int](nTasks)
  private[this] val eFree = Array.ofDim[Long](nTasks)
  
  //  TT structure, needed by ttEn primitive
  private[this] val ttBeforeSMin = Array.ofDim[Long](nTasks)
  private[this] val ttBeforeEMax = Array.ofDim[Long](nTasks)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  @inline private final def ttEn(a: Int, b: Int): Long = ttBeforeEMax(b) - ttBeforeSMin(a)
  
  final override def propagate(): Unit = {
    updateCache()
    C = capacity.max
    
    removeExtremal() // faster
    //removeImpossible()
    
    val limit = toConsider.limit.value    
    
    val nActive = filterActiveSort(sortedBySMin, activeBySMin, smin)
                  filterActiveSort(sortedByEMax, activeByEMax, emax)
    val nMandatory = filterMandatorySort(sortedByEMin, mandatoryByEMin, emin)
                     filterMandatorySort(sortedBySMax, mandatoryBySMax, smax)
    
    // initialize TT : ttBefore smin/emax
    var sminp, smaxp, eminp, emaxp = 0
    var minHeight, e = 0L
    var maxHeight = 0
    var date, prevDate = if (nActive > 0) smin(activeBySMin(0)) else 0  // first event should be first smin
    
    while (emaxp < nActive) {  // do all events until last one = last emax
      // find next date
      date = emax(activeByEMax(emaxp))
      if (sminp < nActive)    date = min(date, smin(activeBySMin(sminp)))
      if (smaxp < nMandatory) date = min(date, smax(mandatoryBySMax(smaxp)))
      if (eminp < nMandatory) date = min(date, emin(mandatoryByEMin(eminp)))
      
      // update energy
      // e += minHeight * (date - prevDate)
      e += (minHeight + max(0L, C - maxHeight)) * (date - prevDate)
      
      // process events 
      while (sminp < nActive && smin(activeBySMin(sminp)) == date) {
        val a = activeBySMin(sminp)
        ttBeforeSMin(a) = e
        maxHeight += hmin(a)
        sminp += 1
      }
      
      while (emaxp < nActive && emax(activeByEMax(emaxp)) == date) {
        val a = activeByEMax(emaxp)
        ttBeforeEMax(a) = e
        maxHeight -= hmin(a)
        emaxp += 1
      }
      
      while (smaxp < nMandatory && smax(mandatoryBySMax(smaxp)) == date) {
        val a = mandatoryBySMax(smaxp)
        minHeight += hmin(a)
        smaxp += 1
      }
      
      while (eminp < nMandatory && emin(mandatoryByEMin(eminp)) == date) {
        val a = mandatoryByEMin(eminp)
        minHeight -= hmin(a)
        eminp += 1
      }
      
      if (minHeight > C) throw Inconsistency
      
      prevDate = date
    }
    
    // initialize free parts
    var i = nActive
    while (i > 0) {
      i -= 1
      val a = activeBySMin(i)
      
      if (smax(a) < emin(a) && required(a)) {
        smaxF(a) = emin(a)
        eminF(a) = smax(a)
        dminF(a) = dmin(a) - (emin(a) - smax(a))
        eFree(a) = hmin(a).toLong * dminF(a)
      }
      else {
        smaxF(a) = smax(a)
        eminF(a) = emin(a)
        dminF(a) = dmin(a)
        eFree(a) = hmin(a).toLong * dmin(a)
      }
    }

    var oldMaxX = nActive - 1
    var end = Int.MaxValue
    
    var y = nActive - 1
    while (y >= 0) {
      val b = activeByEMax(y)
      
      end = emax(b) 
      var minBegin = Int.MaxValue ; var u = -1
      var E, energyReqU = 0L
      
      while (oldMaxX >= 0 && smin(activeBySMin(oldMaxX)) >= end) oldMaxX -= 1
      
      var x = oldMaxX
      while (x >= 0) {
        val a = activeBySMin(x)
        val begin = smin(a)
        
        if (required(a)) {
          if (smax(a) + dmin(a) <= end) E += eFree(a)  // the whole free task is in [ begin ; end [
          else {
            val energyIn = hmin(a).toLong * max(0, end - smaxF(a))   // minimal length, rightmost placement in [ begin ; end [
            E += energyIn
            val energyReqA = min(eFree(a), hmin(a).toLong * (end - smin(a))) - energyIn  // additional if leftmost placement
            if (energyReqA > energyReqU) {
              u = a
              energyReqU = energyReqA
            }
          }
        }
        else if (possible(a)) {
          val energyReqA = min(eFree(a), hmin(a).toLong * (end - smin(a)))
          if (energyReqA > energyReqU) {
            u = a
            energyReqU = energyReqA
          }
        }
      
        val reserve = C * (end - begin) - E - ttEn(a, b)
        if (reserve < 0) throw Inconsistency
        
        if (reserve < energyReqU) {
          if (required(u)) {
            val allowedLength = (reserve + hmin(u).toLong * max(0, min(dmin(u), end - smax(u)))) / hmin(u)
            val newSMin = end - allowedLength
            
            if (smin(u) < newSMin)
              starts(u).updateMin(newSMin.toInt)
          }
          else { // possible(u)
            val newSMin = end - reserve / hmin(u) 
            
            if (smax(u) < newSMin) {
              resources(u).removeValue(id)
              possible(u) = false
              energyReqU = 0L
            } 
          }
        }

        x -= 1
      }
      
      do { y -= 1 } while (y > 0 && emax(activeByEMax(y)) == end)   // find next emax
    }
  }
}


object TimeTableEdgeFinding {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) =
    new TimeTableEdgeFinding(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
}
