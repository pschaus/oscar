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


import oscar.algo.reversible.{ReversibleInt, ReversibleSet, ReversibleSparseSet, ReversibleSparseSetJava}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
  * Arborescence Constraint:
  * A directed graph in which, for any other vertex v != root,
  * there is exactly one directed path from u to root as defined by preds (predecessors).
  * @param preds, the parent of every node
  * @param root the root of the arborescence, preds[root] = root
  * @see CPPropagStrength
  * @author Pierre Schaus pschaus@gmail.com
  * @author Ratheil Houndji  ratheilsesse@gmail.com
  */
final class Arborescence(preds: Array[CPIntVar], root: Int) extends Constraint(preds(0).store, "Circuit") {

  override def associatedVars(): Iterable[CPVar] = preds

  require(preds.length > 0, "no variable.")

  private[this] val n = preds.size
  private[this] val localRoot = Array.tabulate(n)(i => new ReversibleInt(s, i))
  private[this] val leafNodes = Array.tabulate(n)(i => new ReversibleSparseSetJava(s,0,n-1,true))


  final override def setup(l: CPPropagStrength): Unit = {
    for (i <- 0 until n; if i != root) {
      leafNodes(i).insert(i)
    }
    // Create the self loop on the root
    preds(root).assign(root)
    // Attach callback on bind events
    for (i <- 0 until n; if i != root) {
      preds(i).removeValue(i)
      if (preds(i).isBound)
        valBindIdx(preds(i), i)
      preds(i).filterWhenBind() { bind(i) }
    }
  }

  private[this] val values = Array.ofDim[Int](n)
  
  private def bind(i: Int): Boolean = {

    val j = preds(i).min

    val newLocalRoot = localRoot(j)

    // we could remove i from leafNodes(newLocalRoot) but we can't with a reversible sparse-set

    var s = leafNodes(i).fillArray(values)
    while (s > 0) {
      s -= 1
      val l = values(s)
      localRoot(l).value = newLocalRoot
      leafNodes(newLocalRoot).insert(l)
      preds(newLocalRoot).removeValue(l)
    }
    false
  }  
}