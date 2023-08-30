package oscar.algo.search

import oscar.algo.vars.FloatVarLike

/**
  * A trait to be mixed-in a DFSearchNode to make it accept to "branch", or to "constraint" on IntVarLike instances.
  */
trait FloatConstrainableContext extends ConstrainableContext {
  /**
    * Post x == v
    */
  def assign(x: FloatVarLike, v: Double): Unit

  /**
    * Post x <= v
    */
  def smallerEq(x: FloatVarLike, v: Double): Unit

  /**
    * Post x >= v
    */
  def largerEq(x: FloatVarLike, v: Double): Unit
}
