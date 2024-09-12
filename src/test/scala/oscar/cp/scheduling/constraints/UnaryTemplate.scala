package oscar.cp.scheduling.constraints

import oscar.algo.SortUtils._
import Math._

import oscar.cp.core.CPStore
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.scheduling.util.OpenSparseSet

// @author Steven Gay steven.gay@uclouvain.be


class UnaryTemplate(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int, name: String = "Unary")(implicit store: CPStore)
extends Constraint(store, name) {
  private[this] val n = starts.length
  require(n == durations.length)
  require(n == ends.length)
  require(n == resources.length)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ resources

  def setup(strength: CPPropagStrength): Unit = {
    for (a <- 0 until n) {
      if (resources(a).hasValue(id)) {
        if (!resources(a).isBound) {
          resources(a).callPropagateWhenBind(this)
          val cond = { resources(a).hasValue(id) }
          if (!starts(a).isBound)    starts(a)   .callPropagateWhenBoundsChange(this, cond)
          if (!durations(a).isBound) durations(a).callPropagateWhenBoundsChange(this, cond)
          if (!ends(a).isBound)      ends(a)     .callPropagateWhenBoundsChange(this, cond)
        }
        else {
          if (!starts(a).isBound)    starts(a)   .callPropagateWhenBoundsChange(this)
          if (!durations(a).isBound) durations(a).callPropagateWhenBoundsChange(this)
          if (!ends(a).isBound)      ends(a)     .callPropagateWhenBoundsChange(this)
        }
      }
      else toConsider.exclude(a)
    }
    
    propagate()
  }
  
  // Access to inner structure
  final def smin: Array[Int] = sMin
  final def smax: Array[Int] = sMax
  final def emin: Array[Int] = eMin
  final def emax: Array[Int] = eMax
  final def dmin: Array[Int] = dMin
  final def dmax: Array[Int] = dMax
  final def required: Array[Boolean] = requiredTasks
  final def possible: Array[Boolean] = possibleTasks
  
  private[this] val sMin = new Array[Int](n)
  private[this] val sMax = new Array[Int](n)
  private[this] val eMin = new Array[Int](n)
  private[this] val eMax = new Array[Int](n)
  private[this] val dMin = new Array[Int](n)
  private[this] val dMax = new Array[Int](n)
  private[this] val requiredTasks = new Array[Boolean](n)
  private[this] val possibleTasks = new Array[Boolean](n)
  
  private[this] val rToUpdate = new OpenSparseSet(n)
  private[this] val rByStatus = rToUpdate.sortedByStatus
  
  private[this] val dToUpdate = new OpenSparseSet(n)  // durations decoupled from s/e
  private[this] val dByStatus = dToUpdate.sortedByStatus
  
  private[this] val tToUpdate = new OpenSparseSet(n)  // time variables: s/e coupled since constant durations are common
  private[this] val tByStatus = tToUpdate.sortedByStatus
  
  final class OpenSparseSetMod(n: Int) extends OpenSparseSet(n: Int) {
    override def exclude(a: Int) = {
      super.exclude(a)
      rToUpdate.exclude(a)
      dToUpdate.exclude(a)
      tToUpdate.exclude(a)
    } 
  }
  
  val toConsider = new OpenSparseSetMod(n)
  val activitiesToConsider = toConsider.sortedByStatus

  private def updateResource() = {
    var p = rToUpdate.limit.value - 1
    while (p >= 0) {
      val a = rByStatus(p)
      requiredTasks(a) = resources(a).isBoundTo(id)
      
      if (requiredTasks(a)) {
        rToUpdate.exclude(a)
        possibleTasks(a) = true
      }
      else if (!resources(a).hasValue(id)) {
        toConsider.exclude(a)
        possibleTasks(a) = false
      }
      else {
        possibleTasks(a) = true
      }
      p -= 1
    }
  }
  
  
  private def updateDurations() = {
    var p = dToUpdate.limit.value - 1
    while (p >= 0) {
      val a = dByStatus(p)
      dMax(a) = durations(a).max
      
      if (dMax(a) == 0) {
        toConsider.exclude(a)
      }
      else {
        dMin(a) = durations(a).min
        if (dMin(a) == dMax(a)) dToUpdate.exclude(a)
      }
      p -= 1
    }
  }
  
  val identity = Array.tabulate(n)(i => i)
  
  private def updateStartsEnds() = {
    var p = tToUpdate.limit.value
    while (p > 0) {
      p -= 1
      val id = tByStatus(p)
      val dMinTask = dMin(id)

      if (dMinTask == dMax(id)) { // s and e are strongly linked
        val sMinTask = starts(id).min
        val eMaxTask = ends(id).max
        val eMinTask = sMinTask + dMinTask
        sMin(id) = sMinTask
        eMax(id) = eMaxTask  
        sMax(id) = eMaxTask - dMinTask
        eMin(id) = eMinTask   
        if (eMinTask == eMaxTask) tToUpdate.exclude(id)
      }
      else {
        val startVar = starts(id)
        val endVar = ends(id)
        val sMinTask = startVar.min
        val eMaxTask = endVar.max
        sMin(id) = sMinTask
        eMax(id) = eMaxTask     
        sMax(id) = startVar.max 
        eMin(id) = endVar.min       
        if (sMinTask + dMinTask == eMaxTask) tToUpdate.exclude(id)
      }
    }
  }
  
  final def updateCache() = {
    // Step 1: filter out activities not in this resource
    updateResource()
    
    // Step 2: update durations 
    updateDurations()
    
    // Step 3: update starts/ends
    // We link the update for starts and ends because it is very common
    // we could also split the two if starts and ends often became bound independently.
    updateStartsEnds()
  }
  
  
  
  // remove extremal activities from consideration, TT dominance.
  final def removeExtremal() = {
    // get sMin and emax of the task set formed by unfixed activities.  
    var minSMinNotFixed = Int.MaxValue
    var maxEMaxNotFixed = Int.MinValue
    val limit = toConsider.limit.value - 1
    var p = limit
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (sMin(a) + dMin(a) < eMax(a) || !requiredTasks(a)) {  // a not fixed. With variable durations, we can't test it with sMin == sMax 
        minSMinNotFixed = min(minSMinNotFixed, sMin(a))
        maxEMaxNotFixed = max(maxEMaxNotFixed, eMax(a))
      }
      p -= 1
    }
      
    // exclude from consideration all activities that are strictly before min sMin(unbound) or after max eMax(unbound)
    p = limit
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (eMax(a) <= minSMinNotFixed || sMin(a) >= maxEMaxNotFixed) toConsider.exclude(a)
      p -= 1
    }
  }
  
  // Stronger removal than removeExtremal.
  // This removes tasks that can not intersect with other tasks. 
  final def removeIsolated(sortedBySMin: Array[Int], sortedByEMax: Array[Int], nToFilter: Int) = {
    var pSMin, pEMax, overlapMax, overlapCount = 0
    
    while (pEMax < nToFilter) {
      // find next event's date
      var eventDate = emax(sortedByEMax(pEMax))
      if (pSMin < nToFilter) eventDate = min(eventDate, smin(sortedBySMin(pSMin)))
      
      // event treatment
      while (pEMax < nToFilter && emax(sortedByEMax(pEMax)) == eventDate) {
        pEMax += 1
        overlapCount -= 1
      }
      
      if (overlapCount == 0) {
        if (overlapMax == 1) toConsider.exclude(sortedByEMax(pEMax - 1))
        overlapMax = 0
      }
      
      while (pSMin < nToFilter && smin(sortedBySMin(pSMin)) == eventDate) {
        pSMin += 1
        overlapCount += 1
      }      
    }
  }

}
