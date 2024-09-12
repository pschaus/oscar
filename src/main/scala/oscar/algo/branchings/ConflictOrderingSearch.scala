package oscar.algo.branchings


import oscar.algo.reversible.ReversibleInt
import oscar.algo.search._
import oscar.algo.vars.IntVarLike

/**
  * Search described in:
  *
  * "Conflict ordering search for scheduling problems." CP 2015.
  * Gay, Steven, Renaud Hartert, Christophe Lecoutre, and Pierre Schaus.
  *
  * The idea is to order the variables by latest conflict
  */
object ConflictOrderingSearch {
  def apply[T](variables: Array[IntVarLike], varHeuristic: (Int) => T, valHeuristic: (Int) => Int)(implicit orderer: T => Ordered[T]) =
    new ConflictOrderingSearch(variables, varHeuristic, valHeuristic, orderer)
}

/**
  * @author Pierre Schaus pschaus@gmail.com
  */
class ConflictOrderingSearch[T](variables: Array[IntVarLike], varHeuristic: (Int) => T, valHeuristic: (Int) => Int, orderer: T => Ordered[T]) extends Branching {

  require(variables.length > 0)

  private[this] val context = variables(0).context

  var lastDepth = 0
  val depth = new ReversibleInt(context, 0)

  private[this] val priority = Array.ofDim[Long](variables.length)
  private[this] var timestamp = 1L;
  private[this] var lastVar = -1
  private[this] val nUnbound = new ReversibleInt(context, variables.length)
  private[this] val unbound = Array.tabulate(variables.length)(i => i)

  // Last successful assigned value for each variable
  private[this] val lastValues = Array.fill(variables.length)(Int.MinValue)


  override def reset() = {
    lastDepth = 0
    java.util.Arrays.fill(priority, 0, priority.length, 0L)
    java.util.Arrays.fill(lastValues, 0, lastValues.length, Int.MinValue)
    timestamp = 1L;

  }

  def createChildren(varIdx: Int) = {
    val x = variables(varIdx)
    val value = valHeuristic(varIdx)
    branch {
      val ko = isInconsistent(context.assign(x, value))
      if (!ko) lastValues(varIdx) = value
    } {
      val ko = isInconsistent(context.remove(x, value))
      if (!ko && x.isBound) {
        lastValues(varIdx) = x.min
      }
    }
  }

  override def alternatives(): Seq[Alternative] = {

    timestamp += 1

    val d = depth.incr

    // Step 1: if last conflicting variable is new, set it with highest priority
    if (d <= lastDepth) {
      if (lastVar != -1) {
        priority(lastVar) = timestamp
        lastVar = -1
      }
    }
    lastDepth = d

    // Step 2: if some variable in conflict set is not bound, branch on it
    var highestPriorUnbound = -1
    var nU = nUnbound.value
    var i = nU
    var highestPrior = 0L
    while (i > 0) {
      i -= 1
      if (!variables(unbound(i)).isBound) {
        if (priority(unbound(i)) > highestPrior) {
          highestPriorUnbound = unbound(i)
          highestPrior = priority(unbound(i))
        }
      } else {
        nU -= 1
        val tmp = unbound(i)
        unbound(i) = unbound(nU)
        unbound(nU) = tmp
      }
    }
    nUnbound.value = nU
    if (highestPriorUnbound != -1) {
      lastVar = highestPriorUnbound
      val value = valHeuristic(highestPriorUnbound)
      return createChildren(highestPriorUnbound)
    }


    // Step 3: if all conflict set variables are bound, ask heuristic
    if (nU > 0) {
      var bestVar = unbound(0)
      var bestScore = varHeuristic(unbound(0))
      var p = variables.length
      i = nU
      while (i > 0) {
        i -= 1
        val score = varHeuristic(unbound(i))
        if (orderer(score) <= bestScore) {
          bestVar = unbound(i)
          bestScore = score
        }
      }
      lastVar = bestVar
      createChildren(bestVar)
    }
    else {
      // all variables are bound, solution!
      lastVar = -1
      noAlternative
    }
  }
}
