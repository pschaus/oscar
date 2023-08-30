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


import oscar.algo.SortUtils
import oscar.algo.array.{ArrayHeapInt, ArrayStackInt}
import oscar.algo.reversible.{ReversibleArrayStack, ReversibleInt}
import oscar.cp._
import oscar.cp.core._

/**
  * The IDStockingCost constraint holds when each item is produced before
  * its due date ($X_i <= d_i$), the capacity of the machine is respected
  * (i.e. no more than $c$ variables $X_i$ have the same value), and $H$
  * is an upper bound on the total stocking cost ($sum_i((d_i - X_i)*h_i) <= H$).
  *
  * This constraint is the generalization of StockingCost constraint to
  * item dependent stocking cost and useful for modeling
  * Production Planning Problem such as Lot Sizing Problems
  *
  * @param Y        , the variable $Y_i$ is the date of production of item $i$ on the machine
  * @param deadline , the integer $deadline_i$ is the due-date for item $i$
  * @param h        , the integer $h_i$ is the stocking cost for item $i$
  * @param H        , the variable $H$ is an upper bound on the total number of slots all the items are need in stock.
  * @param cap      , the integer $cap_t$ is the maximum number of items the machine can produce during one time slot $t$ (capacity),
  *                 if an item is produced before its due date, then it must be stocked.
  *
  * @author Vinasetan Ratheil Houndji, ratheilesse@gmail.com1
  * @author Pierre Schaus, pschaus@gmail.com
  */
class IDStockingCost(val Y: Array[CPIntVar], val deadline: Array[Int], val h: Array[Int], val H: CPIntVar, val cap: Array[Int]) extends Constraint(Y(0).store, "IDStockingCost") {

  priorityL2 = 0

  this.idempotent = false

  val allDiffBC = new AllDiffBC(Y)

  val n = Y.size
  val X = Array.tabulate(n)(Y(_))
  val Xmax = Array.ofDim[Int](n)
  val aux = Array.ofDim[Int](n + 1)
  val runs = Array.ofDim[Int](n + 1)


  var domMaxMax = Y.map(_.max).max
  var domMinMin = Y.map(_.min).min

  val c = Array.tabulate(domMaxMax + 1)(t => cap(t))

  val optimalSlotTab = Array.fill(n + 1)(-1)
  val optimalItemTab = Array.tabulate(domMaxMax + 1)(t => new ReversibleArrayStack[Int](s, c(t)))

  val ordersToSchedule = new ArrayHeapInt(n)

  val candidateTojump = new ArrayStackInt(n)
  val fullSetsStack = new StackOfStackInt(2 * n)
  val gainCostTab = Array.fill(domMaxMax + 1)(0)

  // ---------------- sparse-set for isolating unbounded variables ---------

  val capa = Array.tabulate(domMaxMax + 1)(t => new ReversibleInt(s, c(t)))
  val fixedCost = new ReversibleInt(s, 0)
  val nUnbound = new ReversibleInt(s, n)
  val unBoundIdx = Array.tabulate(n)(i => i)


  def processFixed(): Unit = {
    var nUnboundTmp = nUnbound.value
    var additionalFixedCost = 0
    var i = nUnboundTmp
    while (i > 0) {
      i -= 1
      val idx = unBoundIdx(i)
      if (X(idx).isBound) {
        // we decrease the capa
        capa(X(idx).value).decr()
        // compute the contribution of this item to the objective
        additionalFixedCost += (deadline(idx) - X(idx).value) * h(idx)
        // remove this item from the unbound ones
        val tmp = unBoundIdx(nUnboundTmp - 1)
        unBoundIdx(nUnboundTmp - 1) = idx
        unBoundIdx(i) = tmp
        nUnboundTmp -= 1
      }
    }
    nUnbound.value = nUnboundTmp
    fixedCost.value = fixedCost.value + additionalFixedCost
  }

  override def setup(l: CPPropagStrength) = {
    X(0).store.add(H === -sum(0 until X.size)(i => (X(i) - deadline(i)) * h(i)))

    Y.foreach(_.callPropagateWhenBoundsChange(this))
    H.callPropagateWhenBoundsChange(this)

    propagate()
  }


  override def propagate() = {


    allDiffBC.propagate()

    // ----------- some preprocessing computation --------------------

    processFixed()

    val nU = nUnbound.value

    assert((0 until nU).forall(i => !X(unBoundIdx(i)).isBound))
    assert((nU + 1 until n).forall(i => X(unBoundIdx(i)).isBound))
    assert(fixedCost.value == (0 until n).filter(i => X(i).isBound).map(i => (deadline(i) - X(i).value) * h(i)).sum)


    var i = nU
    while (i > 0) {
      i -= 1
      Xmax(unBoundIdx(i)) = -X(unBoundIdx(i)).max
    }
    SortUtils.mergeSort(unBoundIdx, Xmax, 0, nU, aux, runs)

    assert((0 until nU - 1).forall(i => X(unBoundIdx(i)).max >= X(unBoundIdx(i + 1)).max))

    // put the max as positive values again ... ;-)
    i = nU
    while (i > 0) {
      i -= 1
      Xmax(unBoundIdx(i)) = -Xmax(unBoundIdx(i))
    }


    // ----------- compute Hopt --------------------

    var Hopt = 0

    ordersToSchedule.clear()
    fullSetsStack.reset()

    assert((0 to domMaxMax).forall(t => optimalItemTab(t).isEmpty))

    var k = 0
    while (k < nU) {
      var t = Xmax(unBoundIdx(k))
      var availableCapacity = capa(t).value
      do {
        while (k < nU && Xmax(unBoundIdx(k)) == t) {
          val i = unBoundIdx(k)
          ordersToSchedule.enqueue(-h(i), i)
          k += 1
        }
        if (availableCapacity > 0) {
          val currentInd = ordersToSchedule.dequeue
          optimalSlotTab(currentInd) = t
          optimalItemTab(t).push(currentInd)
          availableCapacity = availableCapacity - 1
          Hopt = Hopt + (deadline(currentInd) - t) * h(currentInd)
        }
        else {
          fullSetsStack.push(t)
          t = t - 1
          while (capa(t).value == 0) t = t - 1
          availableCapacity = capa(t).value
        }
      } while (ordersToSchedule.size > 0)
      fullSetsStack.push(t)
      fullSetsStack.pushStack()
    }

    val Hmin = Hopt + fixedCost.value



    H.updateMin(Hmin)

    // ----------- now compute the gain costs ----------------

    val nFullSet = fullSetsStack.nStacks()
    assert(false) // to be sure it is not activated

    i = 0
    while (i < nFullSet) {
      /* size of current full set */
      val fullSetSize = fullSetsStack.sizeTopStack()
      candidateTojump.clear
      var j = 0
      while (j < fullSetSize) {
        // set t to the next time slot of the current fullSet
        val t = fullSetsStack.pop
        // filter out candidate top candidate items that can not be placed in t
        // such that the one that remains on top is the most costly one that can jump
        while (!candidateTojump.isEmpty && Xmax((candidateTojump.top)) < t) {
          candidateTojump.pop
        }
        if (candidateTojump.isEmpty) {
          gainCostTab(t) = 0
        } else {
          // select one of the most costly item than can jump (i.e. the one on top of the stack)
          val selected = candidateTojump.top
          gainCostTab(t) = gainCostTab(optimalSlotTab(selected)) + (t - optimalSlotTab(selected)) * h(selected)
        }
        val s = optimalItemTab(t).size
        // add the items placed in t in the candidateTojump
        k = 0
        while (k < s) {
          candidateTojump.push(optimalItemTab(t).pop)
          k += 1
        }
        j += 1
      }
      i += 1
    }

    assert((0 to domMaxMax).forall(t => optimalItemTab(t).isEmpty))

    //-------------- actual prunning the  X based on gain costs ----------

    k = 0
    while (k < nU) {
      i = unBoundIdx(k)
      val lb = optimalSlotTab(i) - (H.max + gainCostTab(optimalSlotTab(i)) - Hmin) / h(i)
      X(i).updateMin(lb)
      k += 1
    }
  }

  def associatedVars() = X :+ H

}

// -----------------------------------
/**
  * Data structure to represent a stack of stack of integer values.
  * For instance [[1,4,2],[5,8]] has two stacks.
  * Its size is 5, the top-post stack has a size of 2.
  * After one pop the status is [[1,4,2],[5]] and the top most stack has size 1
  * After another pop the status is [[1,4,2]] and the top most stack has a size of 3
  * @author Ratheil Houndji
  * @author Pierre Schaus
  */
class StackOfStackInt(n: Int) {

  if (n < 1) throw new IllegalArgumentException("n should be > 0")

  private[this] var mainStack: Array[Int] = Array.ofDim(n)
  private[this] var cumulSizeOfStack: Array[Int] = Array.ofDim(n)

  private[this] var indexMainStack = 0
  private[this] var nStack = 0

  def push(i: Int): Unit = {
    if (indexMainStack == mainStack.length) grow()
    mainStack(indexMainStack) = i
    indexMainStack += 1
  }

  /**
    * close the current stack (if not empty) and start a new empty one
    */
  def pushStack(): Unit = {
    if (indexMainStack != 0 && cumulSizeOfStack(nStack) != indexMainStack) {
      nStack += 1
      cumulSizeOfStack(nStack) = indexMainStack
    }
  }

  def isEmpty(): Boolean = {
    indexMainStack == 0
  }

  def size(): Int = {
    indexMainStack
  }

  /**
    * Pop the top element of the top stack
    * @return the value of the top element on the top stack
    */
  def pop(): Int = {
    if (indexMainStack == 0) throw new NoSuchElementException("Stack empty")
    else {
      if (cumulSizeOfStack(nStack) == indexMainStack) nStack -= 1
      indexMainStack -= 1
      mainStack(indexMainStack)
    }
  }

  /**
    * @return The number of stacks that are stacked
    */
  def nStacks(): Int = {
    nStack
  }

  /**
    * @return The size of the top stack
    */
  def sizeTopStack(): Int = {
    if (cumulSizeOfStack(nStack) == indexMainStack) {
      cumulSizeOfStack(nStack) - cumulSizeOfStack(nStack - 1)
    } else {
      indexMainStack - cumulSizeOfStack(nStack)
    }
  }

  def reset(): Unit = {
    indexMainStack = 0
    nStack = 0
  }

  private def grow(): Unit = {
    val newStack = new Array[Int](indexMainStack * 2)
    System.arraycopy(mainStack, 0, newStack, 0, indexMainStack)
    mainStack = newStack

    val newCumulSizeOfStack = new Array[Int](indexMainStack * 2)
    System.arraycopy(cumulSizeOfStack, 0, newCumulSizeOfStack, 0, indexMainStack)
    cumulSizeOfStack = newCumulSizeOfStack
  }

}