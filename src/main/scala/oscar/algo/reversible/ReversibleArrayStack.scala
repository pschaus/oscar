/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.algo.reversible

/**
 *  An array-based reversible stack for objects.
 *  This means that primitive types are boxed.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleArrayStack[T](node: ReversibleContext, initialSize: Int = 100) {

  private[this] var stack: Array[AnyRef] = Array.ofDim[AnyRef](initialSize)
  private[this] val index = new ReversibleInt(node, 0)

  /**
   *  Return the size of the stack
   *
   *  @return The size of the stack
   */
  @inline final def size: Int = index.value

  /**
   *  Return the size of the stack
   *
   *  @return The size of the stack
   */
  @inline final def length: Int = index.value

  /**
   *  Test if the stack is empty or not
   *
   *  @return `true` if the stack is empty, `false` otherwise
   */
  @inline final def isEmpty = index.value == 0

  /**
   *  Return the top element of the stack without removing it
   *
   *  Throws an exception if the stack is empty
   *
   *  @return The top element of the stack
   */
  @inline final def top: T = stack(index.value - 1).asInstanceOf[T]

  /**
   *  Return the last element of the stack in LIFO order
   *
   *  Throws an exception if the stack is empty
   *
   *  @return The last element of the stack in LIFO order
   */
  @inline final def last: T = {
    if (index.value == 0) sys.error("Stack empty")
    else stack(0).asInstanceOf[T]
  }

  /**
   *  Push an element onto the stack.
   *
   *  @param entry The element to push
   */
  @inline final def push(entry: T): Unit = {
    if (index.value == stack.length) growStack()
    stack(index.value) = entry.asInstanceOf[AnyRef] // boxing in case of primitive type
    index.incr()
  }

  /**
   *  Pop the element on top of the stack
   *
   *  @return The element on top of the stack
   */
  @inline final def pop(): T = {
    if (index.value == 0) sys.error("Stack empty")
    index.decr()
    stack(index.value).asInstanceOf[T]
  }

  /**
   *  Remove all the entries in the stack without removing references
   *  in the internal structure. This means that object cannot be
   *  garbage collected until references are overrided.
   */
  @inline final def clear(): Unit = index.value = 0

  /** Remove all the entries in the stack */
  @inline final def clearRefs(): Unit = {
    var i = index.value
    while (i > 0) {
      i -= 1
      stack(i) = null
    }
    index.value = 0
  }

  @inline final def foreach[U](f: T => U): Unit = {
    var i = index.value
    while (i > 0) {
      i -= 1
      f(stack(i).asInstanceOf[T])
    }
  }
  
  @inline def apply(idx: Int): T = {
    if (idx >= index.value) throw new IndexOutOfBoundsException
    else stack(idx).asInstanceOf[T]
  }

  // Double the size of the stack
  @inline private def growStack(): Unit = {
    val newStack = new Array[AnyRef](stack.length * 2)
    System.arraycopy(stack, 0, newStack, 0, stack.length)
    stack = newStack
  }

  @inline final def toArray: Array[T] = {
    val array = new Array[AnyRef](index)
    System.arraycopy(stack, 0, array, 0, index)
    array.asInstanceOf[Array[T]]
  }
}
