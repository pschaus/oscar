package oscar.algo.search

import oscar.algo.vars.IntVarLike

/**
  * A trait to be mixed-in a DFSearchNode to make it accept to "branch", or to "constraint" on IntVarLike instances.
  */
trait IntConstrainableContext extends ConstrainableContext {

  /**
    * Post x == v
    */
  def assign(x: IntVarLike, v: Int): Unit

  /**
    * Post x != v
    */
  def remove(x: IntVarLike, v: Int): Unit

  /**
    * Post x <= v
    */
  def smallerEq(x: IntVarLike, v: Int): Unit

  /**
    * Post x >= v
    */
  def largerEq(x: IntVarLike, v: Int): Unit

  /**
    * Post x < v
    */
  def smaller(x: IntVarLike, v: Int): Unit = smallerEq(x, v-1)

  /**
    * Post x > v
    */
  def larger(x: IntVarLike, v: Int): Unit = largerEq(x, v+1)

  /**
    * Post x != v for all v in vs
    */
  def remove(x: IntVarLike, vs: Array[Int]): Unit
}
