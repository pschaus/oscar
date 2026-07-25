package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar
import java.util.Set

/**
 * Regular Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
class Regular(val x: Array[CPIntVar], automaton: Automaton) extends Constraint(x(0).store, "Regular") {

  automaton.setPosted() // ensure that the automaton is not modified anymore
  private val nbStates: Int = automaton.getNbStates
  private val T: Array[Array[Int]] = automaton.getTransitionMatrix // transition matrix
  private val initialState: Int = automaton.getInitialState
  private val acceptingStates: Set[Integer] = automaton.getAcceptingStates
  private val q: Array[CPIntVar] = Array.tabulate(x.length)(_ => CPIntVar(0 to nbStates - 1)(s))

  override def associatedVars(): Iterable[CPVar] = x.toSeq ++ q.toSeq

  override def setup(l: CPPropagStrength): Unit = {
    s.post(ElementCst2D(T, CPIntVar(initialState to initialState)(s), x(0), q(0)))

    for (v <- 0 until nbStates) {
      if (!acceptingStates.contains(v)) {
        q(x.length - 1).removeValue(v)
      }
    }
    for (i <- 1 until x.length) {
      s.post(ElementCst2D(T, q(i - 1), x(i), q(i)))
    }
  }
}
