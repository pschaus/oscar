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
 * Reversible Boolean
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleBoolean(context: ReversibleContext, initialValue: Boolean) {



  def this(node: ReversibleContext) = this(node, false)
  
  private[this] var lastMagic: Long = -1L
  private[this] var pointer: Boolean = initialValue

  private[this] val restoreTrue = new TrailEntry {
    override def restore(): Unit = {
      pointer = true;
    }
  }

  private[this] val restoreFalse = new TrailEntry {
    override def restore(): Unit = {
      pointer = false;
    }
  }
  
  @inline private def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      if (this.pointer)
        context.trail(restoreTrue)
      else
        context.trail(restoreFalse)
    }
  }
  
  @inline final def setValue(value: Boolean): Unit = {
    if (value != pointer) {
      trail()
      this.pointer = value
    }
  }
  
  /** @param value to assign */
  @inline final def value_= (value: Boolean): Unit = {
    if (value != pointer) {
      trail()
      this.pointer = value
    }
  }
  
  @inline final def setTrue(): Unit = {
    if (!pointer) {
      trail()
      this.pointer = true
    }
  }
  
  @inline final def setFalse(): Unit = {
    if (pointer) {
      trail()
      this.pointer = false
    }
  }
  
  /** @return current value */
  @inline final def value: Boolean = pointer

  /** @return the current pointer */
  @inline final def getValue(): Boolean = pointer

  override def toString(): String = pointer.toString
}