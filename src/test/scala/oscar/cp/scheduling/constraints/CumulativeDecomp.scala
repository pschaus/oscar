package oscar.cp.scheduling.constraints

import oscar.cp._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.constraints.Sum
import oscar.cp.core.variables.CPVar

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CumulativeDecomp(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) extends Constraint(starts.head.store, "Cumulative decomposition") {

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  private val maxEndDate = maxValue(ends)
  private val minEndDate = minValue(starts)
  private val Horizon = minEndDate to maxEndDate

  private val nTasks = starts.size
  private val Tasks = 0 until nTasks

  private def overlapingTasks(t: Int): IndexedSeq[Int] = {
    Tasks.filter(task => starts(task).min <= t && ends(task).max > t)
  }

  override def setup(l: CPPropagStrength): Unit = {
    for (t <- Horizon) {
      val tasks = overlapingTasks(t)
      val overlapingVars = tasks.map(task => ((starts(task).isLeEq(t) and (ends(task) ?> t) and (resources(task).isEq(id))) * demands(task)))
      val minDemand = minSum(overlapingVars)
      val maxDemand = maxSum(overlapingVars)
      val totDemand = CPIntVar(minDemand to maxDemand)(s)
      s.post(new Sum(overlapingVars.toArray, totDemand))
      s.post(totDemand <= capacity.max)
    }
  }

  private def maxValue(vars: Seq[CPIntVar]): Int = {
    var max = vars.head.max
    vars.tail.foreach(x => max = math.max(max, x.max))
    max
  }

  private def minValue(vars: Seq[CPIntVar]): Int = {
    var min = vars.head.min
    vars.tail.foreach(x => min = math.min(min, x.min))
    min
  }

  private def minSum(vars: Seq[CPIntVar]): Int = {
    var sum = 0
    vars.foreach(sum += _.min)
    sum
  }

  private def maxSum(vars: Seq[CPIntVar]): Int = {
    var sum = 0
    vars.foreach(sum += _.max)
    sum
  }
}