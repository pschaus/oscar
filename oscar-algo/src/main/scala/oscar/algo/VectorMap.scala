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

package oscar.algo

import scala.collection.mutable.ArrayBuffer

/**
 * A VectorMap represents a list of distinct elements of type T with access to their index in O(1). 
 * Elements can not be removed.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
class VectorMap[T] {
  private[this] val map = scala.collection.mutable.Map[T, Int]()
  private[this] val list = ArrayBuffer[T]()
  
  /**
   * Add an element to the list and return its index.
   * If the element is already in the list, only return the index.
   * @param element the element to add.
   * @return the index of the element.
   */
  @inline final def add(element: T): Int = {
    val currentIndex = this.index(element)
    if (currentIndex != -1) {
      return currentIndex
    }
    
    val index = list.size
    list += element
    map(element) = index
    index
  }
  
  /**
   * Get the index of an element if it is in the list, -1 if it's not.
   * @param element the element.
   * @return the index of the element.
   */
  @inline final def index(element: T): Int = map.getOrElse(element, -1)
  
  /**
   * Check whether the list contains an element.
   * @param element the element to check.
   * @return true if the list contains the element, false otherwise.
   */
  @inline final def contains(element: T): Boolean = map.contains(element)
  
  /**
   * Return the element at a certain index.
   * @param index the index of the element.
   * @return the element.
   */
  @inline final def apply(index: Int): T = {
    assert(index >= 0)
    assert(index < list.length)
    list(index)
  }
  
  /**
   * Return the number of element in the list.
   * @return the size
   */
  @inline final def size = list.size
  
  /**
   * Clear the list.
   */
  @inline final def clear(): Unit = {
    list.clear()
    map.clear()
  }
}

object VectorMap {
  def apply[T]() = new VectorMap[T]()
}