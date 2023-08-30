package oscar.algo.branchings

import oscar.algo.search._
import oscar.algo.vars.SetVarLike

class BinarySetBranching(x: SetVarLike) extends Branching {
  private[this] val context = x.context
  def alternatives(): Seq[Alternative] = {
    if (x.isBound) noAlternative
    else {
      val v = x.arbitraryPossibleNotRequired
      branch(context.requires(x, v))(context.excludes(x, v))
    }
  }
}