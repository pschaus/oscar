package oscar.cp.nogoods.searches

import oscar.algo.array.ArrayStack
import oscar.algo.search.DFSearchNode
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.nogoods.decisions.Decision
import oscar.cp.nogoods.core.Nogood
import oscar.cp.nogoods.database.NogoodDB
import oscar.algo.reversible.ReversibleArrayStack

/** @author Renaud Hartert ren.hartert@gmail.com */
class NogoodSearch(store: CPStore, nogoods: NogoodDB) {

  private[this] val branch = new ReversibleArrayStack[(Decision, Boolean)](store, 100)

  private[this] val decisionsStack = new ArrayStack[Iterator[Decision]](100)

  // Number of backtracks of the previous search
  private[this] var nbBkts: Int = 0

  // Number of solutions of the previous search
  private[this] var nbSols: Int = 0

  // Number of nodes explored in the previous search
  private[this] var nbNodes: Int = 0

  // True if the previous search was exhaustive
  private[this] var completed: Boolean = false

  // Actions to execute in case of solution node
  private[this] var solutionActions = List.empty[() => Unit]

  // Actions to execute in case of failed node
  private[this] var failureActions = List.empty[() => Unit]

  /** Returns the number of backtracks in the previous search */
  final def nBacktracks: Int = nbBkts

  /** Returns the number of solutions found in the previous search */
  final def nSolutions: Int = nbSols

  /** Returns the number nodes explored in the previous search */
  final def nNodes: Int = nbNodes

  /** Returns true if the previous search was exhaustive */
  final def isCompleted: Boolean = completed

  /** Adds an action to execute when a failed node is found */
  final def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  /** Clear all actions executed when a solution node is found */
  final def clearOnSolution(): Unit = solutionActions = Nil

  /** Clear all actions executed when a failed node is found */
  final def clearOnFailure(): Unit = failureActions = Nil

  @inline private def expand(branching: NogoodBranching): Boolean = {
    val decision = branching.nextDecision
    if (decision == null) false
    else {
      val decisions = Iterator(decision, !decision)
      decisionsStack.push(decisions)
      true
    }
  }

  final def start(branching: NogoodBranching, stopCondition: NogoodSearch => Boolean): Unit = {

    // Initializes the search
    store.resetStats() // resets trailing time too
    decisionsStack.clear()
    branching.reset() // resets branching
    nbSols = 0
    nbBkts = 0
    nbNodes = 0
    completed = false

    store.pushState()


    // Expand the root node
    if (!store.isFailed) {
      store.pushState()
      val isExpandable = expand(branching)
      if (!isExpandable) {
        store.solFound()
        solutionActions.foreach(_())
        nbSols += 1
      }
    }

    while (!decisionsStack.isEmpty && !stopCondition(this)) {

      nbNodes += 1

      val decisions = decisionsStack.top
      val decision = decisions.next()

      val isLast = !decisions.hasNext

      if (!isLast) store.pushState()
      else decisionsStack.pop() // no more alternative in the sequence

      decision() // apply the alternative

      branch.push((decision, !isLast))

      if (!store.isFailed) {
        val isExpandable = expand(branching)
        if (!isExpandable) {
          store.solFound()
          solutionActions.foreach(_())
          nbSols += 1
          nbBkts += 1
          store.pop()
        }
      } else {
        failureActions.foreach(_())
        nbBkts += 1
        store.pop()
      }
    }

    // Stores the nogoods
    buildNogoods()

    // Pop the remaining nodes 
    var i = decisionsStack.size
    if (i == 0) completed = true
    else while (i != 0) {
      store.pop()
      i -= 1
    }
    store.pop()
    branch.clear()
  }
  
  private[this] val toShave = new ArrayStack[Decision](16)
  final def decisionToShave = toShave

  @inline private def buildNogoods(): Unit = {

    if (branch.isEmpty) {
      //println("UNFEASIBLE <------------------------------------------")
      nogoods.addEmpty()
    }
    else {
      
      val decisions = new Array[(Decision, Boolean)](branch.length)
      var i = decisions.length
      while (i > 0) {
        i -= 1
        decisions(i) = branch.pop
      }

      import scala.collection.mutable.ArrayBuffer
      val positives = ArrayBuffer[Decision]()

      i = 0
      while (i < decisions.length) {
        val (decision, isPositive) = decisions(i)
        if (isPositive) positives.append(!decision)
        else {
          val array = positives.toArray
          val all = array ++ Array(decision)
          //println(all.mkString(" "))
          nogoods.add(new Nogood(all))
        }
        i += 1
      }
    }
  }
}