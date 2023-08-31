package oscar.cp.examples.scheduling

import oscar.cp._
import scala.util.Random
import oscar.cp.scheduling.constraints._


/**
 * @author Steven Gay steven.gay@uclouvain.be
 */
object AlternativeUnary extends CPModel with App {

  val rand = new Random(0)
  val nTasks = 25
  val durationsData = Array.fill(nTasks)(rand.nextInt(10))
  val horizon = durationsData.sum

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val resources = Array.fill(nTasks)(CPIntVar(0, 1))
  val one = CPIntVar(1)
  val demands = Array.fill(nTasks)(one)

  val makespan = maximum(ends)
  val nResources = 2
  
  // Resources
  for (r <- 0 until nResources) {
    add(unaryResource(starts, durations, ends, resources, r))
  }
  
  add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(2)))

  minimize(makespan) search {
//    splitLastConflict(resources ++ starts)
    splitLastConflict(starts ++ resources, i => i)
  }


  println(start())
}
