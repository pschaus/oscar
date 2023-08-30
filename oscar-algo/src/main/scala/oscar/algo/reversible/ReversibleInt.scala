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


package oscar.algo.reversible;

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleInt(context: ReversibleContext, initValue: Int, initSize: Int) extends TrailEntry {
  
  def this(context: ReversibleContext, initValue: Int) = this(context, initValue, 32)
  
  // Inner trailing queue
  private[this] var trail = new Array[Int](initSize)
  private[this] var trailSize = 0
  
  // Current value
  private[this] var pointer: Int = initValue
  
  // Id of the last context
  private[this] var lastMagic: Long = -1L
  
  @inline private def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      if (trailSize == trail.length) growTrail()
      trail(trailSize) = pointer
      trailSize += 1
      context.trail(this)
    }
  }
  
  final override def restore(): Unit = {
    trailSize -= 1
    pointer = trail(trailSize)
  }
  
  /** Increments the reversible integer by one */
  @inline final def incr(): Int = {
    trail()
    pointer += 1
    pointer
  }

  /** Decrements the reversible integer by one */
  @inline final def decr(): Int = {
    trail()
    pointer -= 1
    pointer
  }

  /** Increments the reversible integer by i */
  @inline final def +=(i: Int): Int = {
    trail()
    pointer += i
    pointer
  }

  /** Decrements the reversible integer by i */
  @inline final def -=(i: Int): Int = {
    trail()
    pointer -= i
    pointer
  }
  
  @inline final def setValue(value: Int): Unit = {
    if (value != pointer) {
      trail()
      pointer = value
    }
  }

  /** @param value to assign */
  @inline final def value_= (value: Int): Unit = setValue(value)
  
  /** @param value to assign */
  final def := (value: Int): Unit = setValue(value)
  
  /** @return current value */
  @inline final def value: Int = pointer

  /** @return the current pointer */
  @inline final def getValue(): Int = pointer

  override def toString(): String = pointer.toString
    
  @inline private def growTrail(): Unit = {
    val newTrail = new Array[Int](trailSize * 2)
    System.arraycopy(trail, 0, newTrail, 0, trailSize)
    trail = newTrail
  }
}

object ReversibleInt {
  def apply(value: Int)(implicit context: ReversibleContext) = new ReversibleInt(context, value)
  implicit def reversibleIntToValue(reversible: ReversibleInt): Int = reversible.value
}
