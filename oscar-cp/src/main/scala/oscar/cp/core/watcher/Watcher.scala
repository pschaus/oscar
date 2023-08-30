package oscar.cp.core.watcher

import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar

abstract class Watcher { def awake(): Unit }

final class WatcherL2Garded(constraint: Constraint, gard: => Boolean) extends Watcher {
  private[this] val store = constraint.s
  @inline final override def awake(): Unit = if (gard) store.enqueueL2(constraint)
}

final class WatcherL2(constraint: Constraint) extends Watcher {
  private[this] val store = constraint.s
  @inline final override def awake(): Unit = store.enqueueL2(constraint)
}

final class WatcherL1BindId(constraint: Constraint, variable: CPIntVar, id: Int) extends Watcher {
  private[this] val store = constraint.s
  final override def awake(): Unit = {
    store.enqueueL1(constraint, constraint.priorityBindL1, constraint.valBindIdx(variable, id))
  }
}

final class WatcherL1Bind(constraint: Constraint, variable: CPIntVar) extends Watcher {
  private[this] val store = constraint.s
  final override def awake(): Unit = {
    store.enqueueL1(constraint, constraint.priorityBindL1, constraint.valBind(variable))
  }
}