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

package oscar.algo.reversible

/**
 * A MagicBoolean always gives its default as a value, 
 * unless its value was set within the current magic.
 */
class MagicBoolean(context: ReversibleContext, default: Boolean) {

  private[this] var magic: Long = -1
  private[this] var b: Boolean = default
  
  @inline final def value_=(b: Boolean): Unit = {
    magic = context.magic
    this.b = b
  }
  
  @inline final def value: Boolean = {
    if (magic == context.magic) b
    else default
  }
  
  @inline final def setTrue(): Unit = {
    magic = context.magic
    this.b = true
  }
  
  @inline final def setFalse(): Unit = {
    magic = context.magic
    this.b = false
  }
}