package oscar.algo.branchings

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search._
import oscar.algo.vars.IntVarLike

class BinaryStaticOrderBranching(variables: Array[IntVarLike], valHeuris: (Int => Int)) extends Branching {

  def this(vars: Array[IntVarLike]) = this(vars, vars(_).min)

  private[this] val context = variables(0).context
  private[this] val nVariables = variables.length
  private[this] val depthRev = new ReversibleInt(context, 0)
  private[this] var depth = 0

  final override def alternatives(): Seq[Alternative] = {
    // Cache
    depth = depthRev.value

    // Update depth 
    while (depth < nVariables && variables(depth).isBound) depth += 1

    if (depth == nVariables) noAlternative
    else {
      // Trail new depth
      depthRev.value = depth
      // Alternatives
      val variable = variables(depth)
      val value = valHeuris(depth)
      List(Decision.assign(variable, value), Decision.remove(variable, value))
    }
  }
}