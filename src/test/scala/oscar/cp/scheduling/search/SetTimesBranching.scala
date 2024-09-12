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
 ******************************************************************************/

package oscar.cp.scheduling.search

import oscar.cp._
import oscar.algo.search.Branching
import oscar.algo.reversible.ReversibleInt

/**
 * Set Times Branching
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class SetTimesBranching(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int = (i: Int) => i) extends Branching {

  private[this] val cp = starts(0).store
  private[this] val nTasks = starts.length

  // Tasks not assigned by setTimes
  private[this] val unassigned = Array.tabulate(nTasks)(i => i)
  private[this] val positions = Array.tabulate(nTasks)(i => i)
  private[this] val nUnassigned = new ReversibleInt(cp, nTasks)

  // Old Est of each task
  private[this] val oldEst = Array.fill(nTasks)(new ReversibleInt(cp, Int.MinValue))
  
  // Used to avoid objects and to reduce the number of nodes
  private[this] val dummyAlternative = Seq(() => cp.fail())

  final override def alternatives(): Seq[Alternative] = {
    if (nUnassigned.value == 0) noAlternative
    else {
      val taskId = selectTask()
      val start = starts(taskId)
      val est = start.min
      // 
      if (existsPostponedForever(est)) {
        // we have detected at least one activity that will remain always postponed in this subtree
        dummyAlternative 
      } else {
        val maxEnd = ends(taskId).max
        //println("maxEnd:"+maxEnd)
        //println("minEct:"+selectMinEct(est,taskId))
        
        if (/*false &&*/ selectMinEct(est,taskId) >= maxEnd) {
          branchOne {
            cp.assign(start, est)
            assign(taskId) // the task is assigned by setTimes
          }
        }
          
        else {
          branch {
            val inconsistent = isInconsistent(cp.assign(start, est))
            assign(taskId) // the task is assigned by setTimes
            if (!inconsistent) dominanceCheck()
          } {
            //val minEct = selectMinEct(est)
            cp.propagate()
            oldEst(taskId).value = est
            dominanceCheck()
          }
        }
        


      }
    }
  }
  
  // Return the minimum ect that is greater or equal to value
  @inline private def selectMinEct(value: Int,taskId: Int): Int = {
    var task = nTasks
    var minEct = Int.MaxValue
    while (task > 0) {
      task -= 1
      if (task != taskId) {
        val ect = ends(task).min
        if (ect < minEct && ect > value) minEct = ect
      }

    }
    minEct
  }

  // FIXME: selectTask, dominanceCheck and minUnselectableLst should be 
  //        done in a single pass at the end of each branch
  @inline private def selectTask(): Int = {
    var i = nUnassigned.value
    var minTask = -1
    var minEst = Int.MaxValue
    var minTie = Int.MaxValue
    while (i > 0) {
      i -= 1
      val taskId = unassigned(i)
      val est = starts(taskId).min
      if (oldEst(taskId).value < est || durations(i).max == 0) { // TODO not convinced by durations(i).max == 0
        if (est < minEst) {
          minTask = taskId
          minEst = est
          minTie = tieBreaker(taskId)
        } else if (est == minEst) {
          val tie = tieBreaker(taskId)
          if (tie < minTie) {
            minTask = taskId
            minTie = tie
          }
        }
      }
    }
    minTask // -1 = empty
  }
  
  // FIXME: selectTask, dominanceCheck and minUnselectableLst should be 
  //        done in a single pass at the end of each branch
  @inline private def minUnselectableLst: Int = {
    var i = nUnassigned.value
    var minLst = Int.MaxValue
    while (i > 0) {
      i -= 1
      val taskId = unassigned(i)
      if (oldEst(taskId).value >= starts(taskId).min && durations(taskId).max != 0) {
        val lst = starts(taskId).max
        if (lst < minLst) minLst = lst
      }
    }
    minLst
  }
  
  
  @inline private def existsPostponedForever(est: Int): Boolean = {
    var i = nUnassigned.value
    while (i > 0) {
      i -= 1
      val taskId = unassigned(i)
      if (oldEst(taskId).value >= starts(taskId).min && durations(taskId).max != 0) {
        if (ends(taskId).min <= est || starts(taskId).max <= est)  return true
        // once an activity is postponed, it must always be scheduled after the current est
        // since the current est can only increase down a branch
        // ends(taskId).min <= est  implies that taskId will remain postponed forever
        // starts(taskId).max <= est implies that is would never be possible to schedule taskId after est
        // if one of the two case occurs, we can safely fail    
      }
    } 
    false

  }  

  // FIXME: selectTask, dominanceCheck and minUnselectableLst should be 
  //        done in a single pass at the end of each branch
  @inline private def dominanceCheck(): Unit = {
    val n = nUnassigned.value
    if (n > 0) {
      var failed = true
      var i = n
      while (i > 0 && failed) {
        i -= 1
        val taskId = unassigned(i)
        failed = oldEst(taskId).value >= starts(taskId).min && durations(i).max != 0
      }
      if (failed) cp.fail()
    }
  }

  @inline private def assign(taskId: Int): Unit = {
    val p1 = positions(taskId)
    val p2 = nUnassigned.decr()
    val v2 = unassigned(p2)
    positions(taskId) = p2
    positions(v2) = p1
    unassigned(p1) = v2
    unassigned(p2) = taskId
  }
}

