package oscar.cp.scheduling.constraints

import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.scheduling.util.OpenSparseSet
import Math._

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint

// @author Steven Gay steven.gay@uclouvain.be


class CumulativeTemplate(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
                         heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int, name: String = "Cumulative")
extends Constraint(capacity.store, name) {

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  private[this] val n = starts.length
  require(n == durations.length)
  require(n == ends.length)
  require(n == heights.length)
  require(n == resources.length)

    
  def setup(strength: CPPropagStrength): Unit = {
    for (a <- 0 until n) {
      if (resources(a).hasValue(id) && heights(a).max > 0) {
        if (!resources(a).isBound) {
          resources(a).callPropagateWhenBind(this)
          val cond = { resources(a).hasValue(id) }
          if (!starts(a).isBound)    starts(a)   .callPropagateWhenBoundsChange(this, cond)
          if (!durations(a).isBound) durations(a).callPropagateWhenBoundsChange(this, cond)
          if (!ends(a).isBound)      ends(a)     .callPropagateWhenBoundsChange(this, cond)
          if (!heights(a).isBound)   heights(a)  .callPropagateWhenBoundsChange(this, cond)
        }
        else {
          if (!starts(a).isBound)    starts(a)   .callPropagateWhenBoundsChange(this)
          if (!durations(a).isBound) durations(a).callPropagateWhenBoundsChange(this)
          if (!ends(a).isBound)      ends(a)     .callPropagateWhenBoundsChange(this)
          if (!heights(a).isBound)   heights(a)  .callPropagateWhenBoundsChange(this)
        }
      }
      else toConsider.exclude(a)
    }
    
    if (!capacity.isBound) capacity.callPropagateWhenBoundsChange(this)
    
    propagate()
  }
  
  // Access to inner structure
  final def smin: Array[Int] = sMin
  final def smax: Array[Int] = sMax
  final def emin: Array[Int] = eMin
  final def emax: Array[Int] = eMax
  final def dmin: Array[Int] = dMin
  final def dmax: Array[Int] = dMax
  final def hmin: Array[Int] = hMin
  final def hmax: Array[Int] = hMax
  final def required: Array[Boolean] = requiredTasks
  final def possible: Array[Boolean] = possibleTasks
  
  private[this] val sMin = new Array[Int](n)
  private[this] val sMax = new Array[Int](n)
  private[this] val eMin = new Array[Int](n)
  private[this] val eMax = new Array[Int](n)
  private[this] val hMin = new Array[Int](n)
  private[this] val hMax = new Array[Int](n)
  private[this] val dMin = new Array[Int](n)
  private[this] val dMax = new Array[Int](n)
  private[this] val requiredTasks = new Array[Boolean](n)
  private[this] val possibleTasks = new Array[Boolean](n)
  
  implicit val store = capacity.store
  private[this] val rToUpdate = new OpenSparseSet(n)
  private[this] val rByStatus = rToUpdate.sortedByStatus
  
  private[this] val hToUpdate = new OpenSparseSet(n)
  private[this] val hByStatus = hToUpdate.sortedByStatus
  
  private[this] val dToUpdate = new OpenSparseSet(n)  // durations decoupled from s/e
  private[this] val dByStatus = dToUpdate.sortedByStatus
  
  private[this] val tToUpdate = new OpenSparseSet(n)  // time variables: s/e coupled since constant durations are common
  private[this] val tByStatus = tToUpdate.sortedByStatus
  
  final class OpenSparseSetMod(n: Int) extends OpenSparseSet(n: Int) {
    override def exclude(a: Int) = {
      super.exclude(a)
      rToUpdate.exclude(a)
      hToUpdate.exclude(a)
      dToUpdate.exclude(a)
      tToUpdate.exclude(a)
    } 
  }
  
  val toConsider = new OpenSparseSetMod(n)
  val activitiesToConsider = toConsider.sortedByStatus
  
  // final def nToConsider = toConsider.limit.value


  final def updateResource() = {
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
  
  
  final def updateHeights() = {
    var p = hToUpdate.limit.value - 1
    while (p >= 0) {
      val a = hByStatus(p)
      hMax(a) = heights(a).max
      
      if (hMax(a) == 0) {
        toConsider.exclude(a)
      }
      else {
        hMin(a) = heights(a).min
        if (hMin(a) == hMax(a)) hToUpdate.exclude(a)
      }
      p -= 1
    }
  }
  
  
  final def updateDurations() = {
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
  
  @inline private def updateStartsEnds() = {
    var p = tToUpdate.limit.value
    
    while (p > 0) {
      
      p -= 1
      val taskId = tByStatus(p)
      val dMinTask = dMin(taskId)

      if (dMinTask == dMax(taskId)) { // s and e are strongly linked
        val sMinTask = starts(taskId).min
        val eMaxTask = ends(taskId).max
        val eMinTask = sMinTask + dMinTask
        sMin(taskId) = sMinTask
        eMax(taskId) = eMaxTask  
        sMax(taskId) = eMaxTask - dMinTask
        eMin(taskId) = eMinTask   
        if (eMinTask == eMaxTask) tToUpdate.exclude(taskId)
      }
      else {
        val startVar = starts(taskId)
        val endVar = ends(taskId)
        val sMinTask = startVar.min
        val eMaxTask = endVar.max
        sMin(taskId) = sMinTask
        eMax(taskId) = eMaxTask     
        sMax(taskId) = startVar.max 
        eMin(taskId) = endVar.min       
        if (sMinTask + dMinTask == eMaxTask) tToUpdate.exclude(taskId)
      }
    }
  }
  
  def updateCache() = {
    // Step 1: filter out activities not in this resource
    updateResource()
    
    // Step 2: update heights, filter out activities of height 0
    updateHeights()
    
    // Step 3: update durations, 
    // TODO: basically the same as heights... do a common class? 
    // Exclusion based on height could be done in a second step...
    // It is the same as exclusion on duration!
    updateDurations()
    
    // Step 4: update starts/ends
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
      if (sMin(a) + dMin(a) < eMax(a) || hMin(a) < hMax(a) || !requiredTasks(a)) {  // a not fixed. With variable durations, we can't test it with sMin == sMax 
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
  

  val tcBySMin = Array.ofDim[Int](n)
  val tcByEMax = Array.ofDim[Int](n)
  // remove tasks that can never be pushed by cumulative
  // this computes the pessimistic profile, and removes tasks that are always under the worst profile's limit 
  final def removeImpossible() = {
    val C = capacity.min  // pessimistic
    var q = 0
    var p = toConsider.limit.value - 1
    while (p >= 0) {
      tcBySMin(q) = activitiesToConsider(p)
      tcByEMax(q) = activitiesToConsider(p)
      q += 1
      p -= 1
    }
    
    mergeSort(tcBySMin, sMin, 0, q)
    mergeSort(tcByEMax, eMax, 0, q)
    
    var sMinp = 0
    var eMaxp = 0
    var lastOver = Int.MinValue
    var lastUnder = Int.MinValue + 1
    var height = 0
    
    // build possible profile
    while (eMaxp < q) {
      val oldHeight = height
      var date = eMax(tcByEMax(eMaxp))
      if (sMinp < q) date = min(date, sMin(tcBySMin(sMinp)))
      
      // down events, check if task was under limit during its domain, lower height
      while (eMaxp < q && eMax(tcByEMax(eMaxp)) == date) {
        val a = tcByEMax(eMaxp)
        if (lastUnder > lastOver && lastUnder <= sMin(a)) toConsider.exclude(a)        
        height -= hMax(a)
        eMaxp += 1
      }
      
      // up events, raise height
      while (sMinp < q && sMin(tcBySMin(sMinp)) == date) {
        val a = tcBySMin(sMinp)
        height += hMax(a)
        sMinp += 1
      }
      
      // remember last time profile was under/over limit
      if (oldHeight <= C && height >  C) lastOver = date
      if (oldHeight >  C && height <= C) lastUnder = date
    }
  }
  
  
  private val indices = new Array[Int](n)
  private val temp1   = new Array[Int](n + 1)
  private val temp2   = new Array[Int](n + 1)
  
  // filters out tasks that should not be considered, then sorts them. Returns the number of items in filtered list.
  final def filterFreeSort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Int = {
    val nKeys = byKey.length
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = 0 
    var count = 0
    
    // extract only tasks to consider, that are optional or have a free part
    while (p < nKeys) {
      val task = byKey(p)
      if (status(task) < limit && 
          (!required(task) || smin(task) + dmin(task) < emax(task))) {
        filtered(count) = task
        indices(count) = p
        count += 1
      }
      p += 1
    }
    
    // sort them
    mergeSort(filtered, keys, 0, count, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = count
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
    
    count
  }
  
  // filters out tasks that should not be considered, then sorts them. Returns the number of items in filtered list.
  final def filterActiveSort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Int = {
    val nKeys = byKey.length
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = 0 
    var count = 0
    
    // extract only tasks to consider
    while (p < nKeys) {
      val task = byKey(p)
      if (status(task) < limit) {
        filtered(count) = task
        indices(count) = p
        count += 1
      }
      p += 1
    }
    
    // sort them
    mergeSort(filtered, keys, 0, count, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = count
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
    
    count
  }
  
  final def filterMandatorySort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Int = {
    val nKeys = byKey.length
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = 0 
    var count = 0
    
    // extract only tasks to consider, that are required and have a fixed part
    while (p < nKeys) {
      val task = byKey(p)
      if (status(task) < limit && required(task) && smax(task) < emin(task)) {
        filtered(count) = task
        indices(count) = p
        count += 1
      }
      p += 1
    }
    
    // sort them
    mergeSort(filtered, keys, 0, count, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = count
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
    
    count
  }


  

}
