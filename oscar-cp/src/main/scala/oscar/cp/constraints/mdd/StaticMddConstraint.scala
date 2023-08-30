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


package oscar.cp.constraints.mdd

/**
  * This class describes the different function that should be implemented to create an mdd constraint
  *
  * Note that the states are computed in a top down pass, followed by a bottom up pass.
  * Then a fixed point algorithm is called on the constraints to refine and prune the MDD
  * The splitted nodes are the "hypothetical new nodes" in the refinement process, they have only 1 in-edge,
  * and we try to prune every out edge of those nodes. If all the out edges of a splitted node are deleted, we
  * can delete the in-edge corresponding to this node in the mdd
  *
  * @author rhenneton romain.henneton@hotmail.fr
  */
abstract class StaticMddConstraint {

  /**
    * This function is called on each node before each step of the fixed point of the refinement algorithm
    * The nodes are traversed in a top down way and this function computes the associated node states in this function
    *
    * @param node
    */
  def computeDownStates(node: StaticMddNode): Unit

  /**
    * This function is called on each node before each step of the fixed point of the refinement algorithm (and after the top-down node state computation)
    * The nodes are traversed in a bottom up way and this function computes the associated node states in this function
    *
    * @param node
    */
  def computeUpStates(node: StaticMddNode): Unit

  /**
    * This function returns true if an edge should be deleted when looking at the top node state, bottom node state,
    * edge value and constraint involved.
    *
    * @param edge
    * @return
    */
  def shouldDeleteEdge(edge: StaticMddEdge): Boolean

  /**
    * Remove all the out-edges of the splitted node that are inconsistent with the constraint
    * (note that the splitted node has only one in-edge, this can be useful to speed up the process)
    *
    * @param splittedNode
    */
  def pruneSplittedNode(splittedNode: StaticMddSplittedNode): Unit

  /**
    * Clear all the states relative to the constraint
    */
  def clearStates(): Unit

  /**
    * Clear the states relative to the constraint and this specific node
    *
    * @param node
    */
  def clearStates(node: GlobalNode): Unit

}