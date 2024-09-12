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

import java.util

/**
  * @author rhenneton romain.henneton@hotmail.fr
  */
class GlobalNode() extends Ordered[GlobalNode]{
  private[this] val id = StaticMddNode.getId()

  def getId() : Long = this.id

  override def compare(that: GlobalNode): Int = {
    if(this.id < that.getId) -1
    else if(this.id > that.getId) 1
    else 0
  }

  override def hashCode(): Int = id.toInt

  override def equals(o: scala.Any): Boolean = o.asInstanceOf[GlobalNode].getId() == this.id

}

class StaticMddNode(val layer : Int) extends GlobalNode{
  private[this] val outEdges : util.TreeMap[Int,StaticMddEdge] = new util.TreeMap[Int,StaticMddEdge]()
  private[this] val inEdges : util.TreeSet[StaticMddEdge] = new util.TreeSet[StaticMddEdge]()

  def strHash() : String = {
    val buf = new java.lang.StringBuffer()
    val outValues = outEdges.keySet().iterator()
    while(outValues.hasNext){
      val value = outValues.next()
      buf.append(value)
      buf.append('#')
      buf.append(outEdges.get(value).bottomNode.getId)
      buf.append('#')
    }
    buf.toString
  }

  def getInSize() : Int = this.inEdges.size()

  def getOutSize() : Int = this.outEdges.size()

  def getOutEdge(value : Int) : StaticMddEdge = this.outEdges.get(value)

  def addOutEdge(value : Int, edge : StaticMddEdge) : Unit = this.outEdges.put(value, edge)

  def addInEdge(edge : StaticMddEdge) : Unit = this.inEdges.add(edge)

  def removeOutEdge(value : Int) : Unit = this.outEdges.remove(value)

  def removeInEdge(staticMddEdge: StaticMddEdge) : Unit = this.inEdges.remove(staticMddEdge)

  def peekFirstInEdge() : StaticMddEdge = this.inEdges.first()

  def pollFirstInEdge() : StaticMddEdge = this.inEdges.pollFirst()

  def pollFirstOutEntry() : util.Map.Entry[Int,StaticMddEdge] = this.outEdges.pollFirstEntry()

  def getInEdgeIterator() : util.Iterator[StaticMddEdge] = this.inEdges.iterator()

  def getOutEdgeIterator() : util.Iterator[StaticMddEdge] = this.outEdges.values().iterator()

  def getOutValuesIterator() : util.Iterator[Int] = this.outEdges.keySet().iterator()

  def clearOutEdges() : Unit = this.outEdges.clear()

  def clearInEdges() : Unit = this.inEdges.clear()

  def containsOutValue(value : Int) : Boolean = this.outEdges.containsKey(value)
  /**
    * Pre : The node added and this node have the same destination.
    */
  def merge(addedNode : StaticMddNode) : Unit = {
    if(this.getId != addedNode.getId) {
      /** Change the link of top edges **/
      while (addedNode.getInSize() > 0) {
        val inEdge = addedNode.pollFirstInEdge()
        inEdge.unlink()
        val newEdge = new StaticMddEdge(inEdge.topNode, this, inEdge.value)
      }

      /** Unlink bottom edges **/
      while (addedNode.getOutSize() > 0) {
        val entry = addedNode.pollFirstOutEntry()
        entry.getValue.unlink()
      }
    }
  }

}


object StaticMddNode {
  private[this] var id : Long = 0
  def getId() : Long = {
    id += 1
    id
  }
}

/**
  * This simple object is a temporary node. In the refinement phase, we must sort the potential node
  * splitting based on a certain heuristic, and prune the useless edges if needed. In our case, the splitting
  * corresponds to the extraction of an in-edge. This leads to the splited node object
  * In fact, the splitted node has only one in-edge, and if the splitted node has no more out edges once pruned,
  * this means that the in-edge can be removed directly in the mdd (partial decision in the mdd)
  *
  * @author rhenneton romain.henneton@hotmail.fr
  */
class StaticMddSplittedNode(val inEdge: StaticMddEdge, val originalNode: StaticMddNode) extends GlobalNode {
  /**
    * The outEdges is just a copy of the outEdges of the original nodes.
    */
  private[this] val outEdges: java.util.HashMap[Int, StaticMddNode] = {
    val map: java.util.HashMap[Int, StaticMddNode] = new util.HashMap[Int, StaticMddNode]()
    val originalOutIterator = originalNode.getOutEdgeIterator()
    while (originalOutIterator.hasNext) {
      val edge = originalOutIterator.next()
      map.put(edge.value, edge.bottomNode)
    }
    map
  }

  def getOutEdges(): java.util.HashMap[Int, StaticMddNode] = this.outEdges
}

