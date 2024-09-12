package oscar.cp.scheduling.constraints

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.cp._
import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.CPVar

// @author Steven Gay steven.gay@uclouvain.be

/*
 * Cumulative extended edge finding as in Kameugne et al. Constraints 2014.
 * Here, activities may have alternative resources:
 * this version of the propagator removes optional activities that can not fit among mandatory activities.
 */
class IQuad(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
            demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends Constraint(capacity.store, "IQuad") {
  val n = starts.size
  require(n == durations.size)
  require(n == ends.size)
  require(n == demands.size)
  require(n == resources.size)
  
  val myStore = capacity.store

  val l2r = new IQuadL2R(starts, durations, ends,  demands, resources, capacity, id)
  
  val rs = starts.map(-_).asInstanceOf[Array[CPIntVar]]
  val re = ends  .map(-_).asInstanceOf[Array[CPIntVar]]
  val r2l = new IQuadL2R(re, durations, rs,  demands, resources, capacity, id)

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  def setup(strength: CPPropagStrength): Unit = {
    myStore.post(l2r)
    myStore.post(r2l)

    propagate()
  }
  
  override def propagate() = {
    l2r.propagate()
    r2l.propagate()
  }
}


class IQuadL2R(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
            demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends Constraint(capacity.store, "IQuadL2R") {
  val n = starts.size
  require(n == durations.size)
  require(n == ends.size)
  require(n == demands.size)
  require(n == resources.size)
  
  
  val myStore = capacity.store

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  def setup(strength: CPPropagStrength): Unit = {
    priorityL2 = 2
    
    def boundsCB(v: CPIntVar)  = { if (!v.isBound) v.callPropagateWhenBoundsChange(this) }
    
    def resourceCB(v: CPIntVar) = { if (!v.isBound) v.callPropagateWhenBind(this) }
    
    starts    foreach boundsCB
    durations foreach boundsCB
    ends      foreach boundsCB
    demands   foreach boundsCB
    
    boundsCB(capacity)
    
    resources foreach resourceCB

    propagate()
  }

  
  
  val byEndMax   = Array.tabulate(n)(i => i)
  val byStartMin = Array.tabulate(n)(i => i)
  
  val startsmin = Array.fill(n)(0)
  val endsmax   = Array.fill(n)(0)  

  val rmin    = Array.fill(n)(0)
  val dmax    = Array.fill(n)(0)
  val pmin    = Array.fill(n)(0)
  val cmin    = Array.fill(n)(0)
  
  val mandatory = Array.fill(n)(false)
  val optional  = Array.fill(n)(false)
  
  val Dupd    = Array.fill(n)(0)
  val SLupd   = Array.fill(n)(0)
  val E       = Array.fill(n)(0) 
    
  val nPossible = new ReversibleInt(myStore, n)  // all activities in [nPossible, n) are known to be forbidden in this resource
  val possibleActivities = Array.tabulate(n){ i => i }
  
  def updateCache() = {
    // refresh possible activities ;
    // refresh startmin and end max of possible activities
    var limit = nPossible.value 
    var k = 0
    while (k < limit) {
      val a = possibleActivities(k)
      if (resources(a).hasValue(id)) {
        startsmin(a) = starts(a).min
        endsmax(a)   = ends(a).max 
      }
      else {
        val b = possibleActivities(limit - 1)  // this is inside the array, since 0 <= k < limit
        possibleActivities(limit - 1) = a
        possibleActivities(k) = b
        limit -= 1
        k -= 1
      }
      k += 1
    }
    nPossible.setValue(limit)
    
    
    mergeSort(byStartMin, startsmin)
    mergeSort(byEndMax, endsmax)

    var i = 0
    while (i < n) {
      val ism = byStartMin(i)

      if (resources(ism).hasValue(id)) {
        if (resources(ism).isBoundTo(id)) {
          mandatory(i) = true
          optional(i) = false
        }
        else {
          mandatory(i) = false
          optional(i) = true
        }
        
      }
      else {
        mandatory(i) = false
        optional(i) = false
      }
      
        rmin(i) = startsmin(ism)
        dmax(i) = endsmax(ism)
        pmin(i) = durations(ism).min
        cmin(i) = demands(ism).min
      
        Dupd(i)  = Int.MinValue
        SLupd(i) = Int.MinValue
      
      i += 1
    }
  }
  
  
  override def propagate(): Unit = {
    val C = capacity.max
    
    updateCache()
    
    
    /*
     *  Edge Finding tries to find a task interval \Omega and a task i
     *  s.t. \Omega << i, i.e. all tasks of \Omega finish before the end of i.
     *  Then it updates the est of i using a task interval \Theta \subseteq \Omega.
     *  Kameugne11 does not try to get the tightest possible \Theta,
     *  instead it relies on two heuristics at once :
     *  _ take the \Theta with smallest slack, rho
     *  _ take the \Theta with highest density, tau
     *  This actually converges to the same fixpoint as classic EF with optimal \Theta. 
     */

    var i = 0
    var iUpdatableMax = 0  // improvement: if rmin(i) >= du, no interval task [x, du) can update activity i
    
    var k = 0
    while(k < n) {
    if (resources(byEndMax(k)).hasValue(id) && cmin(byEndMax(k)) > 0) {
      val du = endsmax(byEndMax(k))               // du is the upper bound of \Omega
      while (iUpdatableMax < n && rmin(iUpdatableMax) < du) iUpdatableMax += 1
      
      var Energy = 0
      
      // Step 1.1: compute E(i), the energy between rmin(i) and du 
      i = iUpdatableMax - 1
      while (i >= 0) {
        if (mandatory(i) && dmax(i) <= du) Energy += pmin(i) * cmin(i)
        E(i) = Energy
        i -= 1
      }
      
      i = 1
      while (i < iUpdatableMax) {
        if (rmin(i-1) == rmin(i)) E(i) = E(i-1)
        i += 1
      }

      // Step 1.2: check overload
      i = 0
      while(i < iUpdatableMax) {
    	  if (dmax(i) <= du) {
    		  val emax = C * (du - rmin(i))

 				  if (E(i) > emax)
            throw Inconsistency

				  if (optional(i) && 
  					  E(i) + pmin(i) * cmin(i) > emax
             ) {
            resources(byStartMin(i)).removeValue(id)
            optional(i) = false
				  }
    	  }
        i += 1
      }
      
      
      // Step 2: find strong density updates
      var maxEnergy = 0
      var r_rho = du
      
      i = iUpdatableMax - 1                                                                                                // enumerate on i by decreasing est
      while (i >= 0) {
        val ism = byStartMin(i)
        
        if (mandatory(i)) {
          if (dmax(i) <= du) {
            if (E(i) * (du - r_rho) > maxEnergy * (du - rmin(i))) {   // E(i) / (du-rmin(i)) > maxEnergy/(du-r_rho) => i has higher density
              maxEnergy = E(i)
              r_rho = rmin(i)
            }
          }
          else {                                                                               // \Omega << i automatically: compute the density update for i.
            val rest = maxEnergy - (C - cmin(i)) * (du - r_rho)            // Why not wait for density updates from i' < i?
            if (rest > 0) {                                                                                      // We do density updates only for \Theta \subseteq est_i, lct_i 
              Dupd(i) = math.max(Dupd(i), r_rho + ceiling_div(rest, cmin(i))) 
            }
          
            // additional detection of the journal version
            if (Dupd(i) > rmin(i) && maxEnergy + cmin(i) * (rmin(i) + pmin(i) - r_rho) > C * (du - r_rho))
                starts(ism).updateMin(Dupd(i))
          }
        }
        else if (optional(i)) {
          if (dmax(i) > du) {
            val rest = maxEnergy - (C - cmin(i)) * (du - r_rho)            // Why not wait for density updates from i' < i?
            if (rest > 0) {                                                                                      // We do density updates only for \Theta \subseteq est_i, lct_i 
              Dupd(i) = math.max(Dupd(i), r_rho + ceiling_div(rest, cmin(i))) 
            }
          
            // additional detection of the journal version, for optional values
            if (Dupd(i) > rmin(i) && // r_rho > Int.MinValue &&
                // Dupd(i) > dmax(i) - pmin(i) &&
                maxEnergy + cmin(i) * (rmin(i) + pmin(i) - r_rho) > C * (du - r_rho) &&
                Dupd(i) > starts(ism).max &&
                isInconsistent(resources(ism).removeValue(id))) {
              optional(i) = false
              throw Inconsistency
            }
          }
        }
        
        i -= 1
      }
      
      var minSL = Int.MaxValue ; var r_tau = du
      i = 0
      while (i < iUpdatableMax) {
        val ism = byStartMin(i)
        
        if (mandatory(i) && C * (du - rmin(i)) - E(i) < minSL) {
          r_tau = rmin(i)
          minSL = C * (du - r_tau) - E(i) 
        }
        
        if ((mandatory(i) || optional(i)) && dmax(i) > du) {
          val restp = cmin(i) * (du - r_tau) - minSL
          if (r_tau <= du && restp > 0)
            SLupd(i) = math.max(SLupd(i), r_tau + ceiling_div(restp, cmin(i)))
            
          if (rmin(i) + pmin(i) >= du || minSL < cmin(i) * pmin(i)) {
            val newlbpi = math.max(Dupd(i), SLupd(i))
                        
            if (newlbpi > rmin(i)) {
              if(mandatory(i))
                starts(ism).updateMin(newlbpi)
              if (optional(i) && newlbpi > starts(ism).max) {
                optional(i) = false
                resources(ism).removeValue(id)
              }
            }
          }
        }
        
        i += 1
      }
      }
      k += 1
    }
  }
  
  
    // Taken from CPIntVarViewTimes
	@inline
	def ceiling_div(a: Int, b:Int) = {
      val q = a / b
      if (a > 0 && q * b != a) q + 1
      else q
  }
 
}


object IQuad {
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar], dem: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id :Int): Constraint = {
    new IQuad(s, d, e, dem, r, capacity, id)
  }
}

