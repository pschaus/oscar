package oscar.cp.scheduling.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import java.lang.Math._

import oscar.cp.isInconsistent
import oscar.algo.Inconsistency


// @author Steven Gay steven.gay@uclouvain.be


/*
 * This cumulative constraint enforces the time-tabling rule.
 * Instead of using Beldiceanu's all-tasks sweeping algorithm,
 * it sweeps every task in turn against the profile.
 * It is generalized to variable durations and optional activities.
 */

final class TTPerTask(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "TTPerTask")
{
  priorityL2 = 4
  idempotent = true
  
  // Fast access to cache  
  private[this] val nTasks = starts.length
  private[this] val sMin = smin
  private[this] val sMax = smax
  private[this] val dMin = dmin
  private[this] val dMax = dmax
  private[this] val eMin = emin
  private[this] val eMax = emax
  private[this] val hMin = hmin
  private[this] val hMax = hmax
  private[this] val requiredTasks = required
  private[this] val possibleTasks = possible
  private[this] val actives = activitiesToConsider
  private[this] var C = 0
  
  // Used for inner fixed-point
  private[this] var hasChanged = true
  
  // Profile
  private[this] val profile = new ProfileStructure(sMin, sMax, dMin, eMin, eMax, hMin, requiredTasks, possibleTasks)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  final override def propagate(): Unit = {
    updateCache()
    C = capacity.max

    do {
      hasChanged = false
      if (oneSweep())
        throw Inconsistency
    } while (hasChanged)
    
    if (C == capacity.min) removeExtremal()
    else removeImpossible()
    
    if (filterPossibleActivities())
      throw Inconsistency
  }
  
  @inline private def oneSweep(): Boolean = {
    profile.rebuild(toConsider)
    val maxHeight = profile.maxHeight()
    if (maxHeight > C) true // Check overload 
    else pushAll(maxHeight)
  }
  
  @inline private def pushAll(maxHeight: Int): Boolean = {
    
    val minPushable = C - maxHeight
    var p = toConsider.limit.value
    
    while (p > 0) {
      
      p -= 1
      val taskId = actives(p)   
      val hMinTask = hMin(taskId)
      val sMinTask = sMin(taskId)
      val sMaxTask = sMax(taskId)
      val dMinTask = dMin(taskId)
      
      if (requiredTasks(taskId) && hMinTask > minPushable) {
        val aContributes = sMaxTask < eMin(taskId)   // compute this before changing bounds
        
        if (sMinTask < sMaxTask) {   // push i to the right
          val profileSMin = profile.sweepLR(taskId, C - hMinTask, aContributes)
          if (!aContributes && profileSMin > sMaxTask) return true
          val newSMin = min(profileSMin, sMaxTask)

          if (newSMin > sMinTask) {
            if (isInconsistent(starts(taskId).updateMin(newSMin)))
              return true
            val eMinTask = eMin(taskId)
            val newEMin = max(newSMin + dMinTask, eMinTask)  // eMin might already be later if duration is not constant
            if (newEMin > eMinTask && isInconsistent(ends(taskId).updateMin(newEMin)))
              return true
            
            sMin(taskId) = newSMin
            eMin(taskId) = newEMin
            
            hasChanged |= sMaxTask < newEMin  // do fixed point only if profile changes
          } 
        }
        
        val eMinTask = eMin(taskId)
        val eMaxTask = eMax(taskId)

        if (eMinTask < eMaxTask) {   // push i to the left
          val profileEMax = profile.sweepRL(taskId, C - hMinTask, aContributes)
          if (!aContributes && profileEMax < eMinTask)
            return true
          val newEMax = max(profileEMax, eMinTask)
        
          if (newEMax < eMaxTask) {
            if (isInconsistent(ends(taskId).updateMax(newEMax)))
              return true
            val newSMax = min(newEMax - dMinTask, sMaxTask)
            if (newSMax < sMaxTask && isInconsistent(starts(taskId).updateMax(newSMax)))
              return true
            
            eMax(taskId) = newEMax
            sMax(taskId) = newSMax
            
            hasChanged |= newSMax < eMinTask
          }
        }
      }
    }
    false
  }
  
  
  // TODO: use answer from profile.sweepXY to directly remove activity from resource.
  // TODO: actually only one sweep is enough to know whether the activity can hold or not,
  // we don't care about having two witnesses.
  // TODO: separate possible from required (in cumulative template?)
  
  
  @inline private def filterPossibleActivities(): Boolean = {
    var p = toConsider.limit.value
    while (p > 0) {
      p -= 1
      val i = activitiesToConsider(p)
      if (possibleTasks(i) && !requiredTasks(i) && hMin(i) > 0) { // && dMin(i) > 0
        
        val newEMin: Int = if (sMin(i) < sMax(i)) {
          val profileSMin = profile.sweepLR(i, C - hMin(i), false)  // push i to the right
          val newSMin = min(profileSMin, sMax(i))

          // if (profileSMin == Int.MaxValue) Int.MinValue else            
          if (newSMin > sMin(i)) max(newSMin + dMin(i), eMin(i))            
          else eMin(i)
          
        }
        else eMin(i)
        
        val newSMax: Int = if (eMin(i) < emax(i)) {
          val profileEMax = profile.sweepRL(i, C - hMin(i), false)  // push i to the left
          val newEMax = max(profileEMax, eMin(i))
          
          // if (profileEMax == Int.MinValue) Int.MaxValue else            
          if (newEMax < emax(i)) min(newEMax - dMin(i), sMax(i))            
          else sMax(i)
        }
        else sMax(i)
        
        if (newSMax < newEMin && profile.maxInterval(newSMax, newEMin) + hMin(i) > C){
          if (isInconsistent(resources(i).removeValue(id)))
            return true
          possibleTasks(i) = false
        } 
      }
    }
    false
  }
}


object TTPerTask {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint =
    new TTPerTask(starts, durations, ends, heights, resources, capacity, id)
}
