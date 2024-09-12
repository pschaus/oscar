package oscar.algo.vars

import oscar.algo.search.SetConstrainableContext

/**
 * A trait that all objects that behave like an SetVar should implement
 */
trait SetVarLike {
  def context: SetConstrainableContext

  /**
    * @return true if the domain of the variable has exactly one value,
    * false if the domain has more than one value
    */
  def isBound: Boolean

  /**
    * Test if a value is in the possible values
    * @param value
    * @return  true if value is in the possible values false otherwise
    */
  def isPossible(value: Int): Boolean

  /**
    * Test if a value is in the required values
    * @param value
    * @return  true if value is in the required values false otherwise
    */
  def isRequired(value: Int): Boolean

  /**
    * @return an arbitrary value from possibleSet-requiredSet, the set of values for which we are
    *         not sure yet if they are in requiredSet set or not
    */
  def arbitraryPossibleNotRequired: Int

  /**
    * @return a random value from possibleSet-requiredSet, the set of values for which we are
    *         not sure yet if they are in requiredSet set or not
    */
  def randomPossibleNotRequired: Int

  /**
    * @return returns the set this variable represents, if it is bound
    */
  def value(): Set[Int]

  /**
    * @return a set of values that are, for sure, at this point in the computation, in the SetVar.
    *         requiredSet is included in possibleSet.
    */
  def requiredSet(): Set[Int]

  /**
    * @return a set of values that may be present in the set at this point of the computation.
    *         Includes all the values in requiredSet.
    */
  def possibleSet(): Set[Int]

  /**
    * @return An iterator over all values in possibleSet but not in requiredSet
    */
  def possibleNotRequiredValues: Iterator[Int]

  /**
    * @return an iterator over the values of requiredSet
    */
  def requiredValues: Iterator[Int]

  /**
    * @return size of possibleSet
    */
  def possibleSize: Int

  /**
    * @return size of requiredSet
    */
  def requiredSize: Int
}