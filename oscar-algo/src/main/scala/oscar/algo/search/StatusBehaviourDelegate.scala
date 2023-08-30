package oscar.algo.search

/**
  * Created by saschavancauwelaert on 22/06/16.
  */
class StatusBehaviourDelegate {

  // Actions to execute in case of solution node
  private[this] var solutionActions = List.empty[() => Unit]

  // Actions to execute in case of failed node
  private[this] var failureActions = List.empty[() => Unit]

  /** Adds an action to execute when a failed node is found */
  final def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  /** Clear all actions executed when a solution node is found */
  final def clearOnSolution(): Unit = solutionActions = Nil

  /** Clear all actions executed when a failed node is found */
  final def clearOnFailure(): Unit = failureActions = Nil

  final def performSolutionActions() = {
    solutionActions.foreach(_())
  }

  final def performFailureActions() = {
    failureActions.foreach(_())
  }
}
