package oscar.cp.scheduling.constraints

import oscar.cp._
import oscar.cp._
import oscar.cp.core._
import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt

import scala.collection.mutable.Set
import java.lang.Math.min
import java.lang.Math.max

import oscar.algo.Inconsistency
import oscar.cp.core.variables.CPVar


// @author Steven Gay steven.gay@uclouvain.be

/*
 * Naive cubic EST/LCT overload checker:
 * For every activities i and j,
 * check that activities in the activity interval i;j
 * do not overload the resource. 
 */

class NFNLCubic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
            heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends Constraint(capacity.store, "NFNLCubic") {
  val mstarts: Array[CPIntVar] = ends   map(-_)
  val mends:   Array[CPIntVar] = starts map(-_)
  
  val l2r = new NFNLCubicLR(starts, durations, ends, heights, resources, capacity, id)
  val r2l = new NFNLCubicLR(mstarts, durations, mends, heights, resources, capacity, id)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  def setup(strength: CPPropagStrength): Unit = {
    l2r.setup(strength)
    r2l.setup(strength)
  }
  
  override def propagate() = {
    l2r.propagate()
    r2l.propagate()
  }
}


class NFNLCubicLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
            heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends Constraint(capacity.store, "NFNLCubicLR") {
  val n = starts.size
  require(n == durations.size)
  require(n == ends.size)
  require(n == heights.size)
  require(n == resources.size)
  
  val myActs = (0 until n) filter { a => resources(a).hasValue(id) && heights(a).max > 0 }

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ heights ++ resources ++ Array(capacity)

  def setup(strength: CPPropagStrength): Unit = {
    priorityL2 = 1
        
    def callbacks(a: Int) = {
      if (!resources(a).isBound) resources(a).callPropagateWhenBind(this)
      
      if (!starts(a).isBound)    starts(a)   .callPropagateWhenBoundsChange(this)
      if (!durations(a).isBound) durations(a).callPropagateWhenBoundsChange(this)
      if (!ends(a).isBound)      ends(a)     .callPropagateWhenBoundsChange(this)
      if (!heights(a).isBound)   heights(a)  .callPropagateWhenBoundsChange(this)
    }
    
    myActs foreach callbacks
    
    if (!capacity.isBound) capacity.callPropagateWhenBoundsChange(this)
    
    propagate()
  }
  
  val smin = Array.fill(n)(0)
  val smax = Array.fill(n)(0)
  val emin = Array.fill(n)(0)
  val emax = Array.fill(n)(0)
  val hmin = Array.fill(n)(0)
  val dmin = Array.fill(n)(0)
  val required = Array.fill(n)(false)

  val Acts = Array.ofDim[Int](n)
  var nActs = 0
  
  def updateCache() = {
    nActs = 0
    for (a <- myActs) {
      required(a) = (resources(a).isBoundTo(id) && heights(a).max > 0)
      if (required(a)) {
        smin(a) = starts(a).min
        smax(a) = starts(a).max
        emin(a) = ends(a).min
        emax(a) = ends(a).max
        hmin(a) = heights(a).min 
        dmin(a) = durations(a).min
        if (dmin(a) > 0 && hmin(a) > 0) {
          Acts(nActs) = a
          nActs += 1
        }
      }
    }
  }
  
  // classic NFNL energy
  private final def getEnergy(a: Int, left: Int, right: Int): Int = {
    if (left <= smin(a) && emax(a) <= right) {
      dmin(a) * hmin(a)
    }
    else 0
  }
  

  
  val bySMin = Array.ofDim[Int](n) 
  val byEMin = Array.ofDim[Int](n)
  val lsrs   = Array.ofDim[Int](n)

  val intervalsLeft   = Array.ofDim[Int](8*n*n)
  val intervalsRight  = Array.ofDim[Int](8*n*n)
  val sortedIntervals = Array.ofDim[Int](8*n*n)
  var nIntervals = 0

  @inline
  private final def addInterval(left: Int, right: Int) = {
    if (left < right) {
      intervalsLeft(nIntervals)  = left
      intervalsRight(nIntervals) = right
      sortedIntervals(nIntervals) = nIntervals
      nIntervals += 1
    }
  }
  
  // generate all intervals of interest without duplicates.
  // We would do this using Scala's default Set implementation, but they are too costly.
  def fillIntervals() = {
    nIntervals = 0
    var i = 0
    var j = 0    
    
    // put all possible intervals
    while (i < nActs) {
      val ai = Acts(i)
      j = 0
      while (j < nActs) {
        val aj = Acts(j)
        
        addInterval(smin(ai), emax(aj))
        
        j += 1
      }
      i += 1
    }
    
    
    // sort them by lexicographic order (left, right) with a stable sort
    mergeSort(sortedIntervals, intervalsRight, 0, nIntervals)
    mergeSort(sortedIntervals, intervalsLeft, 0, nIntervals)
    
    // remove duplicate intervals
    var p = 1
    var q = 0
    while (p < nIntervals) {
      val ap = sortedIntervals(p)
      val aq = sortedIntervals(q)
      
      if (intervalsLeft(ap)  != intervalsLeft(aq) || 
          intervalsRight(ap) != intervalsRight(aq)) {
        q += 1
        sortedIntervals(q) = ap
      }
      p += 1
    }
    
    // println(s"nActs = $nActs, nIntervals = $nIntervals, q = $q")
    if (nIntervals > 0) nIntervals = q + 1
    
  }
  
  override def propagate(): Unit = {
    val C = capacity.max
    updateCache()
    
    // Step 1: initialize intervals
    fillIntervals()
    
    // Step 2: generate prunable events by startmin, energy events by endmin.
    System.arraycopy(Acts, 0, bySMin, 0, nActs)
    oscar.algo.SortUtils.mergeSort(bySMin, smin, 0, nActs)
    
    System.arraycopy(Acts, 0, byEMin, 0, nActs)
    oscar.algo.SortUtils.mergeSort(byEMin, emin, 0, nActs)
    
    // Step 3: filtering
    // for every interval
    // remove useless pruning events (i.e. startmin >= right)
    // for every pruning event ev s.t. emin > left
    //   introduce all energy events strictly after ev
    //   check not first condition
    
    var interval = 0
    while (interval < nIntervals) {
      val left  = intervalsLeft(sortedIntervals(interval))
      val right = intervalsRight(sortedIntervals(interval)) 
      
      val energyLimit = C * (right - left)
      var energy = 0
      var eetOmega = Int.MaxValue
      var pruningEvent = nActs - 1
      var energyEvent  = nActs - 1

      // remove useless pruning events
      while (smin(bySMin(pruningEvent)) >= right) pruningEvent -= 1

      // introduce pruning events by decreasing smin
      while (pruningEvent >= 0) {
        val a = bySMin(pruningEvent)
        if (emin(a) > left) {
          val sm = smin(a)
          
          // introduce energy events with emin(_) > sm
          while (energyEvent >= 0 && emin(byEMin(energyEvent)) > sm) {
            val b = byEMin(energyEvent)
            lsrs(b) = getEnergy(b, left, right)  // remember for use in checking below, when the smin event comes
            energy += lsrs(b)
            eetOmega = math.min(eetOmega, emin(b))
            energyEvent -= 1
          }
          
          // check overload
          if (energy > energyLimit) throw Inconsistency
          
          // check not first condition and push a to the right if necessary
          // it is quite sad that start(a) can be pushed to emin(a), but it is correct
          if (energy - lsrs(a) + hmin(a) * (math.min(emin(a), right) - left) > energyLimit) {
            starts(a).updateMin(eetOmega)
          }
        }
        pruningEvent -= 1
      }
      
      interval += 1
    }
  }
}


object NFNLCubic {
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar], h: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id :Int): Constraint = {
    new NFNLCubic(s, d, e, h, r, capacity, id)
  }
}


