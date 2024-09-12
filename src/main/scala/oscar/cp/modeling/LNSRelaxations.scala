package oscar.cp.modeling

import oscar.cp.core.CPSol
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint

trait LNSRelaxations {
  
  /**
   * relax randomly k variables in x, others are assigned to the values they have in sol
   */
  def relaxRandomly(x: IndexedSeq[_ <: CPIntVar], sol: CPSol, k: Int): Unit = {
    val cp = x.head.store
    val n = x.size
    val fixed = (0 until n).toSet -- (for (i <- 1 to k) yield scala.util.Random.nextInt(n)).toSet
    cp.post(fixed.map(i => x(i).eq(sol(x(i)))).toArray[Constraint])
  }

}