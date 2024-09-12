package oscar.cp.scheduling.constraints

import oscar.algo.SortUtils
import oscar.cp.scheduling.util.OpenSparseSet
import Math.min
import Math.max
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPStore
import oscar.algo.array.ArrayStack

// Expected usage: update smin/smax...
// Then rebuild profile
// Finally sweep all relevant activities

final class ProfileStructure(
   smin: Array[Int], 
   smax: Array[Int], 
   dmin: Array[Int],
   emin: Array[Int],
   emax: Array[Int],
   hmin: Array[Int], 
   required: Array[Boolean], 
   possible: Array[Boolean])(implicit val store: CPStore) {

  private[this] val nTasks = smax.length

  private[this] val pointTimes = Array.ofDim[Int](nTasks * 2 + 2)  // one point for origin of times, one for ending
  private[this] val pointHeights = Array.ofDim[Int](nTasks * 2 + 2)
  private[this] var nPoints = 1

  // Initial point
  pointTimes(0) = Int.MinValue
  pointHeights(0) = 0

  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  // events
  private[this] val sortedBySMax = Array.tabulate(nTasks)(i => i)
  private[this] val sortedByEMin = Array.tabulate(nTasks)(i => i)
  private var nSorted = 0
  
  private[this] val filteredBySMax = new Array[Int](nTasks)
  private[this] val filteredByEMin = new Array[Int](nTasks)
  private[this] val location = new Array[Int](nTasks)
  
  // range of indexes of interest in sortedBySMax = [smaxMin, smaxMax[
  // indexes outside are in toConsider
  // this is kept incrementally
  private[this] val smaxMinToConsider = new ReversibleInt(store, 0)
  private[this] val smaxMaxToConsider = new ReversibleInt(store, nTasks)
  
  // same for emin
  private[this] val eminMinToConsider = new ReversibleInt(store, 0)
  private[this] val eminMaxToConsider = new ReversibleInt(store, nTasks)
  
  
  def rebuild(toConsider: OpenSparseSet): Unit = {
    /*
     *  Filter tasks by toConsider + has mandatory part, sort, then generate events
     */
    
    // Reset
    nPoints = 1
    nSorted = 0
    
    val status = toConsider.status
    val limit = toConsider.limit.value
    
    // filter and sort smax events
    var smaxMin = smaxMinToConsider.value
    var smaxMax = smaxMaxToConsider.value
    val oldSMaxMin = smaxMin
    var continue = true
    
    while (smaxMin < smaxMax && continue) {
      val task = sortedBySMax(smaxMin)
      if (status(task) < limit) continue = false
      else smaxMin += 1
    }
    if (smaxMin > oldSMaxMin) smaxMinToConsider.setValue(smaxMin)
    
    val oldSMaxMax = smaxMax
    continue = true
    smaxMax -= 1
    while (smaxMin < smaxMax && continue) {
      val task = sortedBySMax(smaxMax)
      if (status(task) < limit) continue = false
      else smaxMax -= 1
    }
    smaxMax += 1
    if (smaxMax < oldSMaxMax) smaxMaxToConsider.setValue(smaxMax)
    
    var p = smaxMin 
    var q = 0
    
    // filter
    while (p < smaxMax) {
      val task = sortedBySMax(p)
      if (status(task) < limit && smax(task) < emin(task) && required(task) && hmin(task) > 0) {
        filteredBySMax(q) = task
        location(q) = p
        q += 1        
      } 
     p += 1
    }    
    
    // sort 
    SortUtils.mergeSort(filteredBySMax, smax, 0, q, temp1, temp2)
    
    // put tasks back sorted for mergeSort's incremental behaviour
    p = q
    while (p > 0) {
      p -= 1
      sortedBySMax(location(p)) = filteredBySMax(p) 
    }

    // filter and sort emin events
    var eminMin = eminMinToConsider.value
    var eminMax = eminMaxToConsider.value
    val oldEMinMin = eminMin
    continue = true
    
    while (eminMin < eminMax && continue) {
      val task = sortedByEMin(eminMin)
      if (status(task) < limit) continue = false
      else eminMin += 1
    }
    if (eminMin > oldEMinMin) eminMinToConsider.setValue(eminMin)
    
    val oldEMinMax = eminMax
    continue = true
    eminMax -= 1
    while (eminMin < eminMax && continue) {
      val task = sortedByEMin(eminMax)
      if (status(task) < limit) continue = false
      else eminMax -= 1
    }
    eminMax += 1
    if (eminMax < oldEMinMax) eminMaxToConsider.setValue(eminMax)
    
    p = eminMin 
    q = 0
    
    // filter
    while (p < eminMax) {
      val task = sortedByEMin(p)
      if (status(task) < limit && smax(task) < emin(task) && required(task) && hmin(task) > 0) {
        filteredByEMin(q) = task
        location(q) = p
        q += 1        
      } 
     p += 1
    }    
    
    // sort
    SortUtils.mergeSort(filteredByEMin, emin, 0, q, temp1, temp2)
    
    // put tasks back sorted for mergeSort's incremental behaviour
    p = q
    while (p > 0) {
      p -= 1
      sortedByEMin(location(p)) = filteredByEMin(p) 
    }
    
    nSorted = q

    // generate events
    var s = 0 // next start
    var e = 0 // next end
    var h = 0 // height
    
    while (e < nSorted) {
      val prevH = h

      // find next event
      var tSweepLine = emin(filteredByEMin(e))
      if (s < nSorted) tSweepLine = min(tSweepLine, smax(filteredBySMax(s)))
      val t = tSweepLine
      
      // add all tasks at smax
      while (s < nSorted && smax(filteredBySMax(s)) == t) {
        h += hmin(filteredBySMax(s))
        s += 1
      }

      // remove all tasks at emin
      while (e < nSorted && emin(filteredByEMin(e)) == t) {
        h -= hmin(filteredByEMin(e))
        e += 1
      }

      // if the profile has changed, register new profile point
      if (h != prevH) {
        pointTimes(nPoints) = t
        pointHeights(nPoints) = h
        nPoints += 1
      }
    }
    
    // add end of time
    pointTimes(nPoints) = Int.MaxValue
    pointHeights(nPoints) = 0
    nPoints += 1
  }
  
  
  /*
   * Functions to manage sweeping.
   * Tasks are sweep left to right (right to left) to find left (right) support.
   * LR (RL) sweeping uses indexBefore (indexAfter) which can be linear time
   * in the worst case, though we could use a log n approach.
   * Here we simply cache the last index that worked for every given task,
   * and try to use it as a hint the best we can.
   * We expect that in many cases, the cache will have the exact index.
   */
  
  private[this] val lastIndexBefore = Array.fill(nTasks)(0)
  private[this] val lastIndexAfter = Array.fill(nTasks)(0)
  
    // returns the profile event at t or the closest before
  @inline private final def indexBefore(a: Int, t: Int): Int = {
    val last = lastIndexBefore(a)
    val now = if (last >= nPoints) indexBeforeLeft(t, nPoints - 1)
    else if (t < pointTimes(last)) indexBeforeLeft(t, last - 1)
    else if (pointTimes(last) < t) indexBeforeRight(t, last + 1)
    else last
    lastIndexBefore(a) = now
    now
  }
  
  @inline private final def indexBeforeLeft(t: Int, hint: Int): Int = {
    var i = hint
    while (t < pointTimes(i))  i -= 1
    i
  }
  
  // returns the profile event at t or the closest before
  @inline private final def indexBeforeRight(t: Int, hint: Int): Int = {
    var i = hint
    while (i < nPoints && pointTimes(i) <= t)  i += 1
    i - 1
  }
  
  // returns the profile event at t or the closest before
  @inline private final def indexAfter(a: Int, t: Int): Int = {
    val last = lastIndexAfter(a)
    val now = if (last >= nPoints) indexAfterLeft(t, nPoints - 1)
    else if (t < pointTimes(last)) indexAfterLeft(t, last - 1)
    else if (pointTimes(last) < t) indexAfterRight(t, last + 1)
    else last
    lastIndexAfter(a) = now
    now
  }

  
  
  // returns the profile event at t or the closest after
  @inline private final def indexAfterLeft(t: Int, hint: Int): Int = {
    var i = hint
    while (i >= 0 && t <= pointTimes(i))  i -= 1
    i + 1
  }
  
  // returns the profile event at t or the closest after
  @inline private final def indexAfterRight(t: Int, hint: Int): Int = {
    var i = hint
    while (pointTimes(i) < t)  i += 1
    i
  }
  
  
  // returns the profile event at t or the closest before
  @inline private final def indexBefore1(t: Int): Int = {
    var i = 1
    while (i < nPoints && pointTimes(i) <= t) {
      i += 1
    } 
    i - 1
  }
  
  // returns the profile event at t or the closest before
  @inline private final def indexAfter1(t: Int): Int = {
    var i = nPoints - 1
    while (i >= 0 && pointTimes(i) > t) {
      i -= 1
    } 
    i + 1
  }
  
  
  // indexBefore by binary search.
  @inline private final def indexBefore2(t: Int): Int = {
    var l = 0
    var r = nPoints - 1
    while (l + 1 < r) {
      val i = (l + r) >> 1   // (l + r) / 2
      if (pointTimes(i) > t) r = i
      else l = i
    }
    l
  }

  // indexAfter by binary search.
  @inline private final def indexAfter2(t: Int): Int = {
    var l = 0
    var r = nPoints - 1
    while (l + 1 < r) {
      val i = (l + r) >> 1   // (l + r) / 2
      if (pointTimes(i) < t) l = i
      else r = i
    }
    r
  }
  
  
  // Given an activity i, check whether it can fit with the mandatory profile at its smin.
  // If it cannot, find the next place where it can.
  // If there is no such place, return something > smax, typically the next profile event
  // In order to detect that a possible activity cannot fit 

  final def sweepLR(a: Int, C: Int, contributes : Boolean): Int = {
    var event  = indexBefore(a, smin(a))  // e = event before or at smin(a)
    
    var checkFrom = Int.MinValue
    val checkUntilDefault =
      if (contributes) smax(a) else max(smax(a) + dmin(a), emin(a))
    var checkUntil = min(checkUntilDefault, emin(a))
    
    while (pointTimes(event) < checkUntil) {
      if (pointHeights(event) > C) {  // always go to conflict
        checkFrom = Int.MaxValue
        checkUntil = checkUntilDefault
      }
      else if (pointTimes(event) < checkFrom) { // if activity was in conflict, go to check
        checkFrom = pointTimes(event)
        val newEMin = max(checkFrom + dmin(a), emin(a)) // if duration is not fixed, maybe emin(a) > smin(a) + dmin(a)
        checkUntil = min(newEMin, checkUntilDefault)
      }
      // otherwise stay in check, nothing to do
      
      event += 1
    }
    
    
    checkFrom
  }
  
  
  // reverse of sweepLR, watch out for < that become >= and the such, since e is not in [s ; e)
  // also, the profile height is at event - 1
  final def sweepRL(a: Int, C: Int, contributes : Boolean): Int = {
    var event  = indexAfter(a, emax(a)) // index at or after emax(a)
    
    var checkFrom = Int.MaxValue  // time where the activity tries to hold right shifted
    val checkUntilDefault =
      if (contributes) emin(a) else min(emin(a) - dmin(a), smax(a))
    var checkUntil = max(checkUntilDefault, smax(a))
    
    while (pointTimes(event) > checkUntil) {
      if (pointHeights(event - 1) > C) {  // always go to conflict
        checkFrom = Int.MinValue
        checkUntil = checkUntilDefault
      }
      else if (pointTimes(event) > checkFrom) { // if activity was in conflict, go to check
        checkFrom = pointTimes(event)
        val newSMax = min(checkFrom - dmin(a), smax(a))  // if duration is not fixed, maybe smax(a) < emax(a) - dmin(a)
        checkUntil = max(newSMax, checkUntilDefault)
      }
      // otherwise stay in check, nothing to do
      
      event -= 1
    }
    
    checkFrom
  }
  
  
  // find minimum height on open interval [time1 ; time2)
  def minInterval(a: Int, time1: Int, time2: Int): Int = {
    // Find first
    var i = indexBefore(a, time1)

    var min = pointHeights(i)

    // Find min
    while (i < nPoints && pointTimes(i) < time2) {
      val h = pointHeights(i)
      if (h < min) min = h
      i += 1
    }

    min
  }
  
  def maxInterval(time1: Int, time2: Int): Int = {
    // Find first before time1
    var i = 1
    while (i < nPoints && pointTimes(i) <= time1) i += 1
    i -= 1

    var max = pointHeights(i)

    // Find min
    while (i < nPoints && pointTimes(i) < time2) {
      val h = pointHeights(i)
      if (h > max) max = h
      i += 1
    }

    max
  }
  
  
  // returns the maximal height of the whole profile
  def maxHeight(): Int = {
    var max = Int.MinValue
    var i = 0
    while (i < nPoints) {
      if (pointHeights(i) > max) max = pointHeights(i)
      i += 1
    }
    max
  }


  // stacks for height and dates, these actually represent one stack of (height, date) pairs
  private[this] val hStack = new Array[Int](nTasks + 1)
  private[this] val dStack = new Array[Int](nTasks + 1)
  private[this] var sPointer = -1
  
  @inline private def unstack(time: Int, height: Int, dur: Int) = {
    var minHeight = Int.MaxValue
    var lastD = time
    while (hStack(sPointer) <= height) {
      val h = hStack(sPointer) // pop
      lastD = dStack(sPointer) // pop
      sPointer -= 1
      
      if (time - lastD >= dur) minHeight = min(minHeight, h)
    }
    
    sPointer += 1
    hStack(sPointer) = height  // push height
    dStack(sPointer) = lastD   // push date  
    minHeight
  }
  
  hStack(0) = Int.MaxValue
  def minHeightOf(a: Int) = {
    val first = emin(a) // this is only used for tasks with no mandatory part
    val last  = smax(a)
    val d = dmin(a)
    
    var i = indexBefore(a, smin(a))  // find plateau where a can start
//    sPointer = 0
//    hStack(0) = Int.MaxValue
    dStack(0) = first
    
    sPointer = 1
    hStack(1) = pointHeights(i)
    dStack(1) = first
    
    i += 1
    
    var minHeight = Int.MaxValue
    
    while (pointTimes(i) < last) {    
      minHeight = math.min(minHeight, unstack(pointTimes(i), pointHeights(i), d))
      i += 1
    }
      
    minHeight = math.min(minHeight, unstack(last, Int.MaxValue - 1, d))
    
    minHeight
  }
  
  def printProfile: Unit = {
    var i = 0
    while (i < nPoints) {
      println(pointTimes(i) + " " + pointHeights(i))
      i += 1
    }
  }
}

