/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.cp.core.domains

import oscar.algo.reversible.{ReversibleContext, ReversiblePointer}

import scala.util.Random

/**
 *  An adaptable sparse integer domain. The structure used to implement the domain
 *  evolves dynamically with the operations applied on the domain.
 *
 *  @author Renaud Hartert
 *  @author Pierre Schaus
 */
class AdaptableIntDomain(override val context: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntDomain {

  // Reversible domain representation
  private val domain: ReversiblePointer[IntervalDomain] = {
    new ReversiblePointer[IntervalDomain](context, new BoundDomain(context, minValue, maxValue))
  }

  @inline
  override final def size: Int = domain.value.size

  @inline
  override final def isEmpty: Boolean = domain.value.isEmpty

  @inline
  override final def isBound: Boolean = domain.value.isBound

  @inline
  override final def max: Int = domain.value.max

  @inline
  override final def min: Int = domain.value.min

  @inline
  override final def randomValue(rand: Random): Int = domain.value.randomValue(rand)

  @inline
  override final def hasValue(value: Int): Boolean = domain.value.hasValue(value)

  var cpt = 0
  
  @inline
  override final def removeValue(value: Int): Unit = {
    val dom = domain.value
    // If sparse representation, remove
    if (dom.isInstanceOf[IntDomain]) {
      dom.asInstanceOf[IntDomain].removeValue(value)
    } else {
      // UpdateMin if possible
      val minValue = dom.min
      if (minValue == value) dom.updateMin(value + 1)
      else {
        // UpdateMax if possible
        val maxValue = dom.max
        if (maxValue == value) dom.updateMax(value - 1)
        else {
          // Dynamically change the representation of the domain
          // - Use a bit vector for a small and dense domain (not yet available)
          // - Otherwise, use a sparse set
          //if (maxValue - minValue >= 32) {
            val sparse = new SparseSetDomain(domain.context, minValue, maxValue)
            domain.value = sparse
            sparse.removeValue(value)
            
          /*} else {
            val sparse = new SingleBitVectorDomain(domain.node, minValue, maxValue)
            domain.value = sparse
            sparse.removeValue(value)
          }*/
        }
      }
    }
  }

  @inline
  override final def assign(value: Int): Unit = domain.value.assign(value)

  @inline
  override final def updateMin(value: Int): Unit = domain.value.updateMin(value)

  @inline
  override final def updateMax(value: Int): Unit = domain.value.updateMax(value)

  @inline
  override final def nextValue(value: Int): Int = domain.value.nextValue(value)

  @inline
  override final def prevValue(value: Int): Int = domain.value.prevValue(value)

  override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = domain.value.delta(oldMin, oldMax, oldSize)

  override def iterator: Iterator[Int] = domain.value.iterator

  override def toString: String = domain.value.toString
}