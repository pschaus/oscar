package oscar.algo.search

import oscar.algo.reversible.ReversibleContext

/**
  * A reversible context that can be "constrainted", and thus can be in a failed state
  */
trait ConstrainableContext extends ReversibleContext {
  /**
    * @return true if this context is in fail state
    */
  def isFailed: Boolean
}
