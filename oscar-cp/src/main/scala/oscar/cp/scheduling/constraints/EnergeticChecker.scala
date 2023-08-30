/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * *****************************************************************************/

package oscar.cp.scheduling.constraints


import oscar.algo.Inconsistency

import scala.math.max
import scala.math.min
import oscar.cp.core._
import oscar.cp.constraints._
import oscar.cp._
import oscar.algo.SortUtils.stableSort
import oscar.cp.core.variables.CPVar

/**
 * @author Steven Gay 
 */

/*
 * Energetic reasoning as defined in section 3.3.6 of Baptiste et al. book "Constraint-based scheduling"
 * + improvements on the choice of bounds by Derrien & Petit CP2013
 */

// reference text nomenclature: release = start, processing = duration, deadline = end, demand = capacity
class EnergeticChecker(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
                           demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
                           extends Constraint(starts.head.store, "EnergeticChecker") {
  private val n = starts.size
  private val Tasks = 0 until n

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  val rstarts:Array[CPIntVar] = starts.map(i => -i)
  val rends:Array[CPIntVar]   =   ends.map(i => -i)
  
  val leftToRight = new EnergeticCheckerLeftToRight(starts, durations,    ends, demands, resources, capacity, id)
  val rightToLeft = new EnergeticCheckerLeftToRight(rends,  durations, rstarts, demands, resources, capacity, id)  
        
  override def setup(l: CPPropagStrength): Unit = {
    priorityL2 = 1
      
    propagate()
    if (isActive) {
      capacity.callPropagateWhenBoundsChange(this)
      for (i <- Tasks) {
        if (!starts(i).isBound)       starts(i).callPropagateWhenBoundsChange(this)
        if (!ends(i).isBound)           ends(i).callPropagateWhenBoundsChange(this)
        if (!durations(i).isBound) durations(i).callPropagateWhenBoundsChange(this)
        if (!demands(i).isBound)     demands(i).callPropagateWhenBoundsChange(this)
        if (!resources(i).isBound) resources(i).callPropagateWhenBind(this)
      }
    }
  }
  
  override def propagate() = {
    leftToRight.propagate()
  }
}


class EnergeticCheckerLeftToRight(startsV: Array[CPIntVar], durationsV: Array[CPIntVar], endsV: Array[CPIntVar],
								  demandsV: Array[CPIntVar], resourcesV: Array[CPIntVar], capacityV: CPIntVar, ressourceId: Int) {
  private val nTasks = startsV.size
  private val Tasks = 0 until nTasks
  
  // accessing min/max/value of variables is expensive enough to justify a cache
  private val startsMin = Array.fill(nTasks)(0)
  private val startsMax = Array.fill(nTasks)(0)
  private val durationsMin = Array.fill(nTasks)(0)
  private val durationsMax = Array.fill(nTasks)(0)
  private val endsMin = Array.fill(nTasks)(0)
  private val endsMax = Array.fill(nTasks)(0)
  private val demandsMin = Array.fill(nTasks)(0)
  private val demandsMax = Array.fill(nTasks)(0)

  def updateCache() = {
    var p = 0
    while (p < nTasks) {
      startsMin(p)    = startsV(p).min
      startsMax(p)    = startsV(p).max
      durationsMin(p) = durationsV(p).min
      durationsMax(p) = durationsV(p).max
      endsMin(p)      = endsV(p).min
      endsMax(p)      = endsV(p).max
      demandsMin(p)   = demandsV(p).min
      demandsMax(p)   = demandsV(p).max
      p += 1
    }
  }
  
  // Derrien - Petit 2013 formula for minimal energy of activity i during [t_1, t_2)
  
  @inline 
  def W_Sh(i: Int, t_1: Int, t_2: Int) = {
    demandsMin(i) * max(0,     durationsMin(i) 
                            min (t_2 - t_1) 
                            min (endsMin(i) - t_1) 
                            min (t_2 - startsMax(i))
                        )
  }  
  
  def unique[T](a: Array[T]) : Array[T] = {
    var i = 0 ; var j = 0 ; val n = a.size
    while (i < n) {
      if (a(i) != a(j)) {
        j += 1
        a(j) = a(i)
      }
      i += 1  
    }
    a.slice(0, j + 1) // let slice take care of the case n == 0    
  }

  
  type T = (Int, Int, Int)
  def merge_unique(a: Array[T], b: Array[T], m: Array[T])(implicit ord: Ordering[T]) = {
    // a is a sorted array of unique elements. Same for b.
    // merge_unique merges a and b into m, without allowing duplicates, and returns the size of the result  
    var ia = 0 ; var ib = 0 ; var im = 0
    val na = a.size ; val nb = b.size
            
    while (ia < na && ib < nb) { // copy loop when both arrays are nonempty
      if (a(ia) == b(ib)) { // if elements are identical, keep only one copy
        m(im) = a(ia)
        ia += 1
        ib += 1
      }
      else if (ord.lt(a(ia), b(ib))) {
        m(im) = a(ia)
        ia += 1
      }
      else {
        m(im) = b(ib)
        ib += 1
      }
      im += 1
    }
      
    // at most one array is nonempty, copy the remainder in m
    Array.copy(a, ia, m, im, na - ia) ; im += na - ia
    Array.copy(b, ib, m, im, nb - ib) ; im += nb - ib
      
    im
  }
  
  
    // finds the first element of a between imin and imax for which f(a(k)) is true.
    // supposes f increasing on the range, if f false on the range, ALWAYS return imax + 1
  def binary_search(a:Array[T], imin: Int, imax:Int, f: (T) => Boolean): Int = {
    if (!f(a(imax))) return (imax + 1)
      
    var min = imin ; var max = imax
    while (min < max) {
      val mid = (min + max) / 2
      if (f(a(mid))) max = mid
      else           min = mid + 1          
    }
    max
  }

    
  def propagate(): Unit = {
    val myTasks = Tasks.filter( i => resourcesV(i).isBoundTo(ressourceId) && demandsV(i).max > 0 && durationsV(i).max > 0 ).toArray
    updateCache()
    
    val multiO_1 = myTasks.map(i => startsMin(i)) ++ myTasks.map(i => startsMax(i)) 
    scala.util.Sorting.quickSort(multiO_1)
    val O_1 = unique(multiO_1)
    
    
    def fillsort(f: (Int) => (Int, Int)) = {
      val ret = myTasks.map (i => { val (eventTime, eventType) = f(i) 
        (eventTime, i, eventType)
      })
      scala.util.Sorting.quickSort(ret)
      ret
    }
    
    val lct = endsMax.max
    
    val L   = fillsort(i => (startsMin(i) + endsMax(i), 1))
    
    val fixedEvents =
      myTasks.map(i => (endsMax(i),   i, 1)) ++
      myTasks.map(i => (endsMin(i),   i, 1)) ++
      myTasks.map(i => (startsMax(i), i, 1))
    scala.util.Sorting.quickSort(fixedEvents)
    val sortedFixedEvents = unique(fixedEvents)
    
    val timeEvents = sortedFixedEvents ++ L // just for allocation... there should be a faster way. 
    
    val capacityMax = capacityV.max
    for (t_1 <- O_1) {
      var slope = capacityMax - myTasks.map(i => W_Sh(i, t_1, t_1 + 1)).sum
      var load = 0 ; var t_2old = t_1 
      val Lp = L.map { case (eventTime, eventActivity, eventType) => 
        (eventTime - t_1, eventActivity, eventType)
      }
            
      var lastEvent  = merge_unique(Lp, sortedFixedEvents, timeEvents) - 1
      var firstEvent = 0
      
      // firstEvent does not always increase with t_1 because elements from L_p may merge with ones from sortedFixedEvents
      // => have to look for it in the whole array
      
      firstEvent = binary_search(timeEvents, firstEvent, lastEvent, (t => t._1 > t_1) )
      lastEvent  = binary_search(timeEvents, firstEvent, lastEvent, (t => t._1 > lct) ) - 1
      
      var i = firstEvent
      while (i <= lastEvent) { 
        val (t_2, eventActivity, eventType) = timeEvents(i)
        load += slope * (t_2 - t_2old)
        if (load < 0)
          throw Inconsistency
        slope -= W_Sh(eventActivity, t_1, t_2 + 1) - 2 * W_Sh(eventActivity, t_1, t_2) + W_Sh(eventActivity, t_1, t_2 - 1)
        // slope += demands(eventActivity).min * eventType
        t_2old = t_2
        i += 1
      }
    }
  }
}
   
object EnergeticChecker {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): EnergeticChecker = {
    val nTasks = starts.size
    if (nTasks == 0) throw new Exception("no tasks")
    else if (ends.size      != nTasks)    throw new Exception("the number of end variables should be "      + nTasks)
    else if (durations.size != nTasks)    throw new Exception("the number of duration variables should be " + nTasks)
    else if (demands.size   != nTasks)    throw new Exception("the number of demand variables should be "   + nTasks)
    else if (resources.size != nTasks)    throw new Exception("the number of resource variables should be " + nTasks)
    else if (durations.exists(_.min < 0)) throw new Exception("durations have to be superior or equal to 0")
    else 
      new EnergeticChecker(starts, durations,ends, demands, resources, capacity, id)
  }
}
