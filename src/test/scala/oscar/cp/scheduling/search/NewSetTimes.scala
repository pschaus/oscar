package oscar.cp.scheduling.search

import oscar.cp._
import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching

/** @author Renaud Hartert */
class NewSetTimes(starts: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int = (i: Int) => i) extends Branching {

  require(starts.length > 0, "no variable")
  
  private[this] val store = starts(0).store
  private[this] val nTasks = starts.length
  
  private[this] val unassigned = Array.tabulate(nTasks)(i => i)
  private[this] val nUnassignedRev = new ReversibleInt(store, nTasks)
  private[this] var nUnassigned: Int = 0 // cache for nUnassignedRev
  
  final override def alternatives(): Seq[Alternative] = {
    updateUnassigned()
    if (nUnassigned == 0) noAlternative
    else if (nUnassigned == 1) singleBranching()
    else binaryBranching()
  }
    
  @inline private def singleBranching(): Seq[Alternative] = {
    val task = starts(unassigned(0))
    nUnassignedRev.decr()
    branchOne(store.assign(task, task.min))
  }
  
  @inline private def binaryBranching(): Seq[Alternative] = {
    val id = selectMinId()
    val task = unassigned(id)
    // Assign the task
    nUnassigned -= 1
    unassigned(id) = unassigned(nUnassigned)
    unassigned(nUnassigned) = task
    // Branching
    val start = starts(task)
    val est = start.min
    val minEct = selectMinEct(est)
    branch { 
      nUnassignedRev.decr() // trully assign the task
      store.assign(start, est) 
    } { store.post(start >= minEct) }
  }
  
  // Remove assigned tasks
  @inline private def updateUnassigned(): Unit = {
    nUnassigned = nUnassignedRev.value
    var i = nUnassigned
    while (i > 0) {
      i -= 1
      val task = unassigned(i)
      if (starts(task).isBound) {
        nUnassigned -= 1
        unassigned(i) = unassigned(nUnassigned)
        unassigned(nUnassigned) = task
      }
    }
  }
  
  // Return the unassigned id of the next task to assign
  @inline private def selectMinId(): Int = {
    var i = nUnassigned
    var minId = -1
    var minEst = Int.MaxValue
    var minTie = Int.MaxValue
    while (i > 0) {
      i -= 1
      val taskId = unassigned(i)
      val est = starts(taskId).min
      if (est < minEst) {
        minId = i
        minEst = est
        minTie = tieBreaker(taskId)
      } else if (est == minEst) {
        val tie = tieBreaker(taskId)
        if (tie < minTie) {
          minId = i
          minTie = tie
        }
      }
    }
    minId // -1 = empty
  }
  
  // Return the minimum ect that is greater or equal to value
  @inline private def selectMinEct(value: Int): Int = {
    var task = nTasks
    var minEct = Int.MaxValue
    while (task > 0) {
      task -= 1
      val ect = ends(task).min
      if (ect < minEct && ect > value) minEct = ect
    }
    minEct
  }
}