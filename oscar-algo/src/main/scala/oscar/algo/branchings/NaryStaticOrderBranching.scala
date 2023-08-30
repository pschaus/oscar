package oscar.algo.branchings

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search._
import oscar.algo.vars.IntVarLike

class NaryStaticOrderBranching(variables: Array[IntVarLike], valOrder: (Int => Seq[Int])) extends Branching {
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
      valOrder(depth).sorted.map((value) => () => {context.assign(variable,value): Unit})
    }
  }
}