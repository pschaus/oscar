package oscar.algo.vars

import oscar.algo.search.FloatConstrainableContext

/**
 * A trait that all objects that behave like an FloatVar should implement
 */
trait FloatVarLike {
  def context: FloatConstrainableContext

  /**
    * @return true if the domain of the variable has exactly one value,
    * false if the domain has more than one value
    */
  def isBound: Boolean

  /**
    * Return a *lower bound* for this expression
    */
  def min: Double

  /**
    * Return a *higher bound* for this expression
    */
  def max: Double

  /**
    * @param v: value to test
    * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
    */
  def isBoundTo(v: Double): Boolean = isBound && hasValue(v)

  /**
    * Test if a value is in the domain
    * @param value: value to test
    * @return  true if the domain contains the value val, false otherwise
    */
  def hasValue(value: Double): Boolean

  /**
    * @return returns the set this variable represents, if it is bound
    */
  def value(): Double

  /**
    * Return a representative name for this var(-like), if one was given
    */
  def name: String
}