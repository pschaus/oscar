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

import oscar.algo.Inconsistency
import oscar.algo.array.ArrayStack

class SearchStatistics(
                        val nNodes: Int,
                        val nFails: Int,
                        val time: Long,
                        val completed: Boolean,
                        val timeInTrail: Long,
                        val maxTrailSize: Int,
                        val nSols: Int) {
  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ntime(ms): $time\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}

/**
 *  DFS search
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class DFSearch(node: DFSearchNode) {

  private[this] val alternativesStack = new ArrayStack[Iterator[Alternative]](100)

  // Number of backtracks of the previous search
  private[this] var nbBkts: Int = 0

  // Number of solutions of the previous search
  private[this] var nbSols: Int = 0

  // Number of nodes explored in the previous search
  private[this] var nbNodes: Int = 0
  
  // True if the previous search was exhaustive
  private[this] var completed: Boolean = false


  // Actions to execute in case of solution node
  private[this] var solutionActions = List.empty[() => Unit]

  // Actions to execute in case of failed node
  private[this] var failureActions = List.empty[() => Unit]

  private[this] var searchListener_ : DFSearchListener = null

  /** Gets the DFSearch listener */
  def searchListener : DFSearchListener = searchListener_

  /** Sets the DFSearch listener */
  def searchListener_= (listener : DFSearchListener) : Unit = searchListener_ = listener

  /** Returns the number of backtracks in the previous search */
  final def nBacktracks: Int = nbBkts

  /** Returns the number of solutions found in the previous search */
  final def nSolutions: Int = nbSols

  /** Returns the number nodes explored in the previous search */
  final def nNodes: Int = nbNodes
  
  /** Returns true if the previous search was exhaustive */
  final def isCompleted: Boolean = completed

  /** Adds an action to execute when a failed node is found */
  final def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  /** Clear all actions executed when a solution node is found */
  final def clearOnSolution(): Unit = solutionActions = Nil

  /** Clear all actions executed when a failed node is found */ 
  final def clearOnFailure(): Unit = failureActions = Nil

  @inline private def expand(branching: Branching): Boolean = {
    val alternatives = branching.alternatives
    if (alternatives.isEmpty) false
    else {
      alternativesStack.push(alternatives.iterator)
      true
    }
  }

  final def start(branching: Branching, stopCondition: DFSearch => Boolean = _ => false/*, searchListener : DFSearchListener*/): Unit = {

    // Initializes the search
    node.resetStats() // resets trailing time too
    alternativesStack.clear()
    branching.reset() // resets branching
    nbSols = 0
    nbBkts = 0
    nbNodes = 0
    completed = false

    if(searchListener_ != null)
      searchListener_.onPush(node)
    node.pushState()

    // Expand the root node
    if (!node.isFailed) {
      if(searchListener_ != null)
        searchListener_.onPush(node)
      node.pushState()
      val isExpandable = expand(branching)
      if (!isExpandable) {
        node.solFound()
        solutionActions.foreach(_())
        nbSols += 1
        node.pop()
      }
    }

    while (!alternativesStack.isEmpty && !stopCondition(this)) {

      nbNodes += 1

      val alternatives = alternativesStack.top
      val alternative = alternatives.next()
      
      val isLast = !alternatives.hasNext
      
      if (!isLast) {
        if(searchListener_ != null)
          searchListener_.onPush(node)
        node.pushState()
      }
      else alternativesStack.pop() // no more alternative in the sequence

      if(searchListener_ != null)
        searchListener_.onBranch(alternative)

      try {
        alternative() // apply the alternative
      } catch {
        case _: Inconsistency => node.fail()
      }

      if (!node.isFailed) {
        val isExpandable = expand(branching)
        if (!isExpandable) {
          node.solFound()
          solutionActions.foreach(_())
          nbSols += 1
          nbBkts += 1
          if(searchListener_ != null)
            searchListener_.onPop(node)
          node.pop()
        }
      }
      else {
        failureActions.foreach(_())
        nbBkts += 1
        if(searchListener != null)
          searchListener.onPop(node)
        node.pop()
      }
    }
    
    // Pop the remaining nodes 
    var i = alternativesStack.size
    if (i == 0) completed = true
    else while (i != 0) {
      if(searchListener_ != null)
        searchListener_.onPop(node)
      node.pop()
      i -= 1
    }
    if(searchListener_ != null)
      searchListener_.onPop(node)
    node.pop()
  }
}