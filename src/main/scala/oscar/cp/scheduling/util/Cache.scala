package oscar.cp.scheduling.util

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversibleInt

/*
 *  This is intended to maintain caches of Arrays of CPIntVar
 */

abstract class Cache() {
  def update(): Unit
  
}

// This one is used in scheduling for resource variables
// We need to update the cache for variable r(i) iff id is possible and not required  
class ResourceCache(rs: Array[CPIntVar], required: Array[Boolean], possible: Array[Boolean], id: Int)(implicit store: CPStore)
extends Cache {
  val n = rs.length
  
  /*
   * resourceStatus(a) < possibleActivities.value => a possible
   */
  val resourceStatus = Array.tabulate(n)(i => i)
  val sortedByStatus = Array.tabulate(n)(i => i)
  val possibleActivities = new ReversibleInt(store, n)
 
  def update() = {
    var p = possibleActivities.value - 1
    while (p >= 0) {
      val a = sortedByStatus(p)
      required(a) = rs(a).isBoundTo(id)
      if (required(a)) {
        possible(a) = true
        removeActivity(a)
      }
      else {
        possible(a) = rs(a).hasValue(id)
        if (!possible(a)) {
          removeActivity(a)
        }
      }
      p -= 1
    }
  }
  
  
  def removeActivity(a: Int) = {
    possibleActivities.decr
    val ib = possibleActivities.value
    val ia = resourceStatus(a)
    
    val b = sortedByStatus(ib)
    sortedByStatus(ib) = a
    sortedByStatus(ia) = b
    resourceStatus(a) = ib
    resourceStatus(b) = ia
  }
}


object ResourceCache {
  def apply(rs: Array[CPIntVar], required: Array[Boolean], possible: Array[Boolean], id: Int)(implicit store: CPStore) = {
    val n = rs.length
    require(n == required.length, "required not the same size as rs")
    require(n == possible.length, "possible not the same size as rs")
    new ResourceCache(rs, required, possible, id)

  } 
  
  def apply(rs: Array[CPIntVar], id: Int)(implicit store: CPStore) = {
    val n = rs.length
    val required = Array.ofDim[Boolean](n)
    val possible = Array.ofDim[Boolean](n)
    new ResourceCache(rs, required, possible, id)
  }
}




class BoundsCache(bs: Array[CPIntVar], mins: Array[Int], maxs: Array[Int])(implicit store: CPStore)
extends Cache {
  val n = bs.length
  
  /*
   * updatableStatus(a) < updatableActivities.value => a is not bound
   */
  val updatableStatus = Array.tabulate(n)(i => i)
  val sortedByStatus = Array.tabulate(n)(i => i)
  val updatableActivities = new ReversibleInt(store, n)
 
  def update() = {
    var p = updatableActivities.value - 1
    while (p >= 0) {
      val a = sortedByStatus(p)
      mins(a) = bs(a).min
      maxs(a) = bs(a).max
      
      if (mins(a) == maxs(a))  removeActivity(a)
      
      p -= 1
    }
  }
  
  def removeActivity(a: Int) = {
    updatableActivities.decr
    val ib = updatableActivities.value
    val ia = updatableStatus(a)
    
    val b = sortedByStatus(ib)
    sortedByStatus(ib) = a
    sortedByStatus(ia) = b
    updatableStatus(a) = ib
    updatableStatus(b) = ia
  }
}


object BoundsCache {
  def apply(bs: Array[CPIntVar], mins: Array[Int], maxs: Array[Int])(implicit store: CPStore) = {
    val n = bs.length
    require(n == mins.length, "mins not the same size as bs")
    require(n == maxs.length, "maxs not the same size as bs")
    new BoundsCache(bs, mins, maxs)

  } 
  
  def apply(bs: Array[CPIntVar])(implicit store: CPStore) = {
    val n = bs.length
    val mins = Array.ofDim[Int](n)
    val maxs = Array.ofDim[Int](n)
    new BoundsCache(bs, mins, maxs)
  }
}



class OpenSparseSet(n: Int)(implicit store: CPStore) {
  
  /*
   * status(a) < limit => a in set
   */
  
  val status = Array.tabulate(n)(i => i)
  val sortedByStatus = Array.tabulate(n)(i => i)
  val limit = new ReversibleInt(store, n)


  def exclude(a: Int) = {
    val ia = status(a)
    if (ia < limit.value) {
      limit.decr
      val ib = limit.value
    
      val b = sortedByStatus(ib)
      sortedByStatus(ib) = a
      sortedByStatus(ia) = b
      status(a) = ib
      status(b) = ia
    }
  }
}


object OpenSparseSet {
  def apply(n: Int)(implicit store: CPStore) = new OpenSparseSet(n) 
}
