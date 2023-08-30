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

package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt

/**
 * Redundant Bin-Packing Flow Constraint
 * @author pschaus@gmail.com
 */
class BinPackingFlowExtended(val x: Array[CPIntVar], val sizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar]) extends Constraint(x(0).store) {

  override def associatedVars(): Iterable[CPVar] = x ++ l ++ c

  val perm = ArrayUtils.sortPerm(sizes);
  val l_t = Array.fill(c.size)(new ReversibleInt(s, 0))
  val c_t = Array.fill(c.size)(new ReversibleInt(s, 0))
  val candidates_t = Array.fill(c.size)(new ReversibleInt(s, 0))
  val permRev = perm.reverse

  override def setup(strength: CPPropagStrength): Unit = {
    x.foreach(_.updateMax(l.length - 1))
    x.foreach(_.updateMin(0))
    s.post(new GCCVarAC(x, 0, c), CPPropagStrength.Strong)

    for (lt <- l)
      lt.callPropagateWhenBoundsChange(this);
    for ((xt, i) <- x.zipWithIndex) {
      if (xt.isBound) {
        val j = xt.min
        l_t(j).setValue(l_t(j).value + sizes(i))
        c_t(j).incr
      } else {
        xt.callValBindIdxWhenBind(this, i);
        xt.callPropagateWhenBind(this);
      }
    }
    for (card <- c) {
      card.callPropagateWhenBoundsChange(this);
      //card.callPropagateWhenBind(this);
    }
    for ((variable,index) <- x.zipWithIndex; if (!variable.isBound)) {
      for (bin <- variable) {
        candidates_t(bin).incr
      }
    }
    propagate()

  }


  override def valRemoveIdx(x: CPIntVar, idx: Int, value: Int): Unit = {
    candidates_t(value).decr()
  }


  override def valBindIdx(x: CPIntVar, idx: Int): Unit = {
    val j = x.min
    val wj = sizes(idx)
    l_t(j).setValue(l_t(j).value + wj)
    c_t(j).incr()
    candidates_t(j).decr()
  }

  override def propagate(): Unit = {
    for (j <- 0 until l.size) {
      setCardinality(j)
      updateLoad(j)
    }
  }

  def bestLoad(j: Int, cardInit: Int, cardToReach: Int, loadInit: Int, permArray: Array[Int]) = {

    var curCard = cardInit
    var curLoad = loadInit
    for (i <- bestCandidatesForBin(j, permArray) if curCard < cardToReach) {
      curLoad += sizes(i)
      curCard += 1
    }

    curLoad
  }

  def updateLoad(j: Int): Unit = {
    // update load min based on card min
    l(j).updateMin(bestLoad(j,c_t(j).value,c(j).min,l_t(j).value,perm))
    // update load max based on card max
    l(j).updateMax(bestLoad(j,c_t(j).value,c(j).max,l_t(j).value,permRev))
  }

  /**
   * Adapt the cardinality of bin j
   * @param j is the bin index
   * @return Failure if fail detected when adapting cards, or Suspend otherwise
   */
  def setCardinality(j: Int): Unit = {
    setMinCard(j)
    setMaxCard(j)
  }

  /**
   * compute the maximum cardinality for the bin `bin`
   */
  def setMaxCard(bin: Int): Unit = {
    val (card, load) = getCard(bin, perm, (binLoad, nextItemSize) => binLoad + nextItemSize <= l(bin).getMax.intValue())
    c(bin).updateMax(card)
  }

  /**
   * compute the minimum cardinality for the bin `bin`
   */
  def setMinCard(bin: Int): Unit = {
    val (card, load) = getCard(bin, permRev, (binLoad, nextItemSize) => l(bin).getMin.intValue() > binLoad)
    if (load < l(bin).getMin.intValue())
      throw Inconsistency
    else
      c(bin).updateMin(card)
  }

  /**
   * compute the minimum cardinality for the bin `bin`
   * @param sorted items is the list of items with items used first at the beginning
   * @param continueLoad(binLoad,nextItemSize) a function that return true if the bin should continue to be loaded
   * @return
   */
  def getCard(bin: Int, sortedItems: Array[Int], continueLoad: (Int, Int) => Boolean): (Int, Int) = {

    var binCompCard = c_t(bin).getValue // the current value of the computation of the cardinality
    var binLoad = l_t(bin).getValue

    for (i<- bestCandidatesForBin(bin,sortedItems) if continueLoad(binLoad, sizes(i))) {
      binLoad += sizes(i)
      binCompCard += 1
    }

    (binCompCard, binLoad)

  }


  val candidatesAvailableForBin = Array.fill(c_t.length)(0)


  /**
   * stream of the item that can go into the bin `bin`
   * An item can be refuted if it make impossible if every previous item are packed in `bin` the bin to fill another one.
   * This is based on the cardinalities of the others bins
   *
   */
  def bestCandidatesForBin(bin:Int,sortedItems: Array[Int]) =
  {
    var itemsListHead = -1; //the last index of sorted item tried in sortedItems
    for (b <- 0 until c_t.size) {
      candidatesAvailableForBin(b) = if (b == bin) 0 else candidates_t(b).value - (c(b).getMin.intValue - c_t(b).getValue)

    }

    def nextAcceptableItem() : Stream[Int] = {
      itemsListHead += 1
      if(itemsListHead == sortedItems.length) Stream.empty
      else {
        val i = sortedItems(itemsListHead)
        if (x(i).hasValue(bin) && !x(i).isBound) {
          val refuteItem = (0 until c_t.length).exists(b => b!= bin &&  x(i).hasValue(b)
            && candidatesAvailableForBin(b) <= 0)
          if(!refuteItem){
            for (b <- 0 until c_t.size; if x(i).hasValue(b))
            {
              candidatesAvailableForBin(b) -= 1
            }
            i #:: nextAcceptableItem
          } else nextAcceptableItem

        } else
          nextAcceptableItem
      }
    }

    nextAcceptableItem

  }


}
