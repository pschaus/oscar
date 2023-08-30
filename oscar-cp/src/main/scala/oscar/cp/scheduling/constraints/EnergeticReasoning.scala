package oscar.cp.scheduling.constraints


import oscar.algo.Inconsistency

import scala.collection.mutable.HashSet
import scala.math.min
import scala.math.max
import scala.math.ceil
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint

class EnergeticReasoning(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int = 1) extends Constraint(capacity.store, "Energetic Reasoning") {

  assert(starts.length == durations.length && starts.length == ends.length && starts.length == demands.length && starts.length == resources.length, "starts, durations, ends, demands and resources must be of same length")

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  //cache arrays
  val numTasks = starts.length
  
  val smin = Array.fill(numTasks)(-1)
  val smax = Array.fill(numTasks)(-1)
  val emin = Array.fill(numTasks)(-1)
  val emax = Array.fill(numTasks)(-1)
  val demmin = Array.fill(numTasks)(-1)
  val durmin = Array.fill(numTasks)(-1)
  
  var cmin = -1
  var cmax = -1
  
  val tasksId = Array.tabulate(starts.length)(i => i)
  
  def setup(l: CPPropagStrength): Unit = {
    priorityL2 = 0
    
    propagate()
    for (task <- tasksId) {
      starts(task).callPropagateWhenBoundsChange(this)
      durations(task).callPropagateWhenBoundsChange(this)
      ends(task).callPropagateWhenBoundsChange(this)
      demands(task).callPropagateWhenBoundsChange(this)
      resources(task).callPropagateWhenBind(this)
    }
    capacity.callPropagateWhenBoundsChange(this)
  }

  override def propagate: Unit = {

    if(s.isFailed)
      throw Inconsistency
    
    //cache
    var i = 0
    while(i < numTasks) {
      smin(i) = starts(i).min
      smax(i) = starts(i).max
      emin(i) = ends(i).min
      emax(i) = ends(i).max
      demmin(i) = demands(i).min
      durmin(i) = durations(i).min
      i += 1
    }
    
    cmin = capacity.min
    cmax = capacity.max

    //keep only the tasks that we know are assigned to the resource id considered by this constraint 
    val tasks = tasksId filter (task => resources(task).isBound && resources(task).min == id && durmin(task) > 0 && demmin(task) > 0)

    val newEets = emin map ( v => v)
    val newLsts = smax map (v => v)

    val intervals = computeIntervals(tasks)
    
    for ((t1, t2) <- intervals) {
      
      val currentIntervalEnergy = energyForInterval(t1, t2, tasks)
      val currentMinIntervalEnergy = cmin * (t2 - t1)
      val currentMaxIntervalEnergy = cmax * (t2 - t1)
      if (currentIntervalEnergy > currentMinIntervalEnergy) {
        capacity.updateMin(ceil(currentIntervalEnergy.toDouble / (t2 - t1)).toInt)
      } else {
        //bound adjustements computation
        for (task <- tasks) {
          val slackWithoutCurrentActivity = currentMaxIntervalEnergy - currentIntervalEnergy + activityEnergyForInterval(task, t1, t2, tasks)

          val leftShiftedEnergy = p_plusForInterval(task, t1, t2, tasks) * demmin(task) 
          val rightShiftedEnergy = p_minusForInterval(task, t1, t2, tasks) * demmin(task)
          
          if (slackWithoutCurrentActivity < leftShiftedEnergy)
            newEets(task) = max(newEets(task), t2 + ceil((leftShiftedEnergy - slackWithoutCurrentActivity).toDouble / demmin(task)).toInt)

          if (slackWithoutCurrentActivity < rightShiftedEnergy)
            newLsts(task) = min(newLsts(task), t1 - ceil((rightShiftedEnergy - slackWithoutCurrentActivity).toDouble / demmin(task)).toInt)

        }
      }
    }

    //apply bound adjustements
    for (task <- tasks) {
      if (smax(task) > newLsts(task))
        starts(task).updateMax(newLsts(task))

      if(emin(task) < newEets(task))
        ends(task).updateMin(newEets(task))
    }
  }

  @inline
  private def computeIntervals(tasks: IndexedSeq[Int]) = {
    
    val (o1,o2,ot) = getO1_O_2_Ot(tasks)
    
    val intervals = HashSet[Tuple2[Int, Int]]()

    for (t1 <- o1; t2 <- o2 if t1 < t2)
      intervals += Tuple2(t1, t2)

    for (o <- ot) {
      for (s <- o1 if (o(s) >= 0 && s < o(s)))
        intervals += Tuple2(s, o(s))

      for (e <- o2 if (o(e) >= 0 && e > o(e)))
        intervals += Tuple2(o(e), e)
    }
    intervals
  }
  
  @inline
  private def getO1_O_2_Ot(tasks: IndexedSeq[Int]) = {

	  val o1 = HashSet[Int]()
	  val o2 = HashSet[Int]()
	  val ot = HashSet[(Int) => Int]()

	  for (task <- tasks) {
		  o1 += smin(task) //est
	  
		  o1 += emin(task) //ect
		  o1 += smax(task) //lst

		  o2 += smax(task) //lst
		  o2 += emin(task) //ect
		  o2 += emax(task) //lct

		  ot += ((t: Int) => smin(task) + emax(task) - t) //est + lct - t
		  
	  }
	  (o1, o2, ot)
  }

  @inline
  private def energyForInterval(t1: Int, t2: Int, tasks: IndexedSeq[Int]) = {
    var energy = 0

    for (task <- tasks)
      energy += activityEnergyForInterval(task, t1, t2, tasks)

    energy
  }

  @inline
  private def p_plusForInterval(task: Int, t1: Int, t2: Int, tasks: IndexedSeq[Int]) = max(0, durmin(task) - max(0, t1 - smin(task))) 

  @inline
  private def p_minusForInterval(task: Int, t1: Int, t2: Int, tasks: IndexedSeq[Int]) = max(0, durmin(task) - max(0, emax(task) - t2))

  @inline
  private def activityEnergyForInterval(task: Int, t1: Int, t2: Int, tasks: IndexedSeq[Int]) = min(t2 - t1, min(p_plusForInterval(task, t1, t2, tasks), p_minusForInterval(task, t1, t2, tasks))) * demmin(task)
}

object EnergeticReasoning {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) =
    new EnergeticReasoning(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
}
