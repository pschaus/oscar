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

import oscar.cp.core.variables.{CPBoolVar, CPIntVar}
import collection.JavaConverters._


/**
  * @author Romain Henneton et Pierre Schaus pschaus@gmail.com
  */
object MddKnapsack {
  def apply(items: Array[CPBoolVar], weights: Array[Int], loads: Set[Int]) = {

    val maxLoad = loads.max

    val mdd = new StaticMddImpl(items.size);
    val arity = items.length

    import scala.collection.mutable.Map
    var mapWeightsToNode = Map[Int,StaticMddNode](0 -> mdd.root)
    var mapNodeToWeight = Map[StaticMddNode,Int](mdd.root -> 0)

    for (l <- 0 until arity - 1) {

      val mapWeightsToNodeNext = Map[Int, StaticMddNode]()
      val mapNodeToWeightNext = Map[StaticMddNode, Int]()

      val ite = mdd.layers(l).iterator()
      while (ite.hasNext()) {
        val n = ite.next()
        val partialWeight = mapNodeToWeight(n)
        for ((w: Int, v: Int) <- Seq((weights(l), 1), (0, 0))) {
          if (partialWeight + w <= maxLoad) {
            val node = mapWeightsToNodeNext.get(partialWeight + w) match {
              case Some(n) => {
                n
              }
              case None => {
                // not selecting the item
                val node = mdd.createNode(l + 1)
                mapWeightsToNodeNext(partialWeight + w) = node
                mapNodeToWeightNext(node) = partialWeight + w
                node
              }
            }
            mdd.addEdge(n, node, v)
          }
        }
      }
      mapWeightsToNode = mapWeightsToNodeNext
      mapNodeToWeight = mapNodeToWeightNext
    }
    // last layer

    val ite = mdd.layers(arity-1).iterator()
    while (ite.hasNext()) {
      val n = ite.next()
      val partialWeight = mapNodeToWeight(n)
      for ((w: Int, v: Int) <- Seq((weights(arity - 1), 1), (0, 0))) {
        if (loads.contains(partialWeight + w)) {
          mdd.addEdge(n, mdd.end, v)
        }
      }
      // todo automate that
      if (n.getOutSize() == 0)  {
        mdd.pushDeadEnd(n)
      }
    }
    mdd.deleteEdgesAndPropagate()
    new Mdd4RConstraint(items.map(_.asInstanceOf[CPIntVar]),mdd,items(0).store)
  }

}
