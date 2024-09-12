package oscar.cp.scheduling.constraints

import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

class UnaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int = 1)
  extends Constraint(starts.head.store, "UnaryResource") {

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ resources


  override def setup(l: CPPropagStrength): Unit = {

    val unitDemand = CPIntVar(1)(s)
    val demands = Array.fill(starts.size)(unitDemand)

    l match {
      case Weak =>
        s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id))
      // steven, stop adding things here, weak = time-table only
      case Automatic =>
        s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id))
        s.post(Unary(starts, durations, ends, resources, id))
      case Medium =>
        s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id))
        s.post(Unary(starts, durations, ends, resources, id))
      case Strong =>
        s.post(TTPerTask(starts, durations, ends, demands, resources, unitDemand, id))
        s.post(Unary(starts, durations, ends, resources, id))
    }
  }
}


