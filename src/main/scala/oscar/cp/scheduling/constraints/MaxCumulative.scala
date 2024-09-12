package oscar.cp.scheduling.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPPropagStrength._
import oscar.cp.scheduling.constraints._

class MaxCumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int = 1)
  extends Constraint(starts.head.store, "Max Cumulative") {

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  override def setup(l: CPPropagStrength): Unit = {
    s.post(new MaxCumulativeCapaCheck(starts,durations,ends,demands,resources,capacity,id))

    l match {
      case Weak =>
        s.post(TTPerTask(starts,durations,ends,demands,resources,capacity,id))
        // steven, stop adding things here, weak = time-table only
      case Automatic =>
        s.post(TTPerTask(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableOverloadChecker(starts,durations,ends,demands,resources,capacity,id))
      case Medium =>
        s.post(TTPerTask(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableDisjunctiveReasoning(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableOverloadChecker(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableEdgeFinding(starts,durations,ends,demands,resources,capacity,id))
      case Strong =>
        s.post(TTPerTask(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableDisjunctiveReasoning(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableOverloadChecker(starts,durations,ends,demands,resources,capacity,id))
        s.post(TimeTableEdgeFinding(starts,durations,ends,demands,resources,capacity,id))
        s.post(EnergeticReasoning(starts,durations,ends,demands,resources,capacity,id))
    }
  }
}

object MaxCumulative {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) =
    new MaxCumulative(starts, durations, ends, demands, resources, capacity, id: Int)
}
