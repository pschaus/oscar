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

package oscar.algo.search

import scala.collection.immutable.LinearSeq

/**
 *  A class to represent an iterator of alternative.
 *  Classical Iterator[Alternative] could be used but 
 *  does not convey the discrepancy information.
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class Alternatives {
  
  def next(): Alternative
  
  def hasNext(): Boolean
  
  def discrepancy: Int
}

final class AlternativeIndexedSeq(alternatives: Seq[Alternative]) extends Alternatives {  
  private var i = 0 
  override final def next(): Alternative = {
    val al = alternatives(i)
    i += 1
    al
  }  
  override final def hasNext: Boolean = i < alternatives.length
  override final def discrepancy: Int = i
}

final class AlternativeLinearSeq(alternatives: Seq[Alternative]) extends Alternatives {
  private var list = alternatives
  private var d = 0
  override final def next(): Alternative = {
    val al = list.head
    list = list.tail
    d += 1
    al
  }
  override final def hasNext: Boolean = !list.isEmpty
  override final def discrepancy: Int = d
}

final class AlternativeArray(alternatives: Array[Alternative]) extends Alternatives {  
  private var i = 0 
  override final def next(): Alternative = {
    val al = alternatives(i)
    i += 1
    al
  }  
  override final def hasNext: Boolean = i < alternatives.length
  override final def discrepancy: Int = i
}

object Alternatives {
  def apply(alternatives: Seq[Alternative]): Alternatives = {
    if (alternatives.isInstanceOf[IndexedSeq[Alternative]]) new AlternativeIndexedSeq(alternatives)
    else new AlternativeLinearSeq(alternatives)
  }
  def apply(alternatives: Array[Alternative]): Alternatives = new AlternativeArray(alternatives)
}
