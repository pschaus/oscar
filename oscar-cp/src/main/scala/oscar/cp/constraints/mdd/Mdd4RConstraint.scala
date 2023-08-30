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

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleContext, ReversibleInt, ReversibleSharedSparseSet}
import oscar.cp._
import oscar.cp.core.variables.CPVar

/**
  * @author rhenneton romain.henneton@hotmail.fr
  */
class Mdd4RConstraint(variables: Array[CPIntVar], mdd: StaticMdd, reversibleContext: ReversibleContext) extends Constraint(variables(0).store, "MddGlobal") {
  idempotent = true

  private[this] val nEdges = mdd.getNumberOfEdges()
  private[this] val nNodes = mdd.getNumberOfNodes()

  private[this] val nVariables = variables.length

  private[this] val staticDomains: Array[util.TreeSet[Int]] = mdd.staticDomains()

  private[this] val nodes: Array[ReversibleMddNode] = Array.ofDim(nNodes)
  private[this] val edges: Array[ReversibleMddEdge] = Array.ofDim(nEdges)
  private[this] val nodesMapping: util.HashMap[Long, Int] = mdd.mapNodes(nodes, reversibleContext)
  private[this] val edgesMapping: util.HashMap[Long, Int] = mdd.mapEdgesAndLink(edges, nodes, nodesMapping)
  private[this] val supports: Array[Array[ReversibleSharedSparseSet]] = setSupports()

  /**
    * Need to have :
    * -Number of edges per layer
    * -Current domain in mdd per layer
    * -Active Nodes per layer
    *
    * => With all of this, everything should be fine.
    **/

  // Values active for each variable in the mdd
  private[this] var activeValues: Array[ReversibleSharedSparseSet] = null
  // Number of remaining edges per layer (need to be set in setup)
  private[this] var edgeCount: Array[ReversibleInt] = null
  // Nodes that are actives on each layer
  private[this] var activeNodes: Array[ReversibleSharedSparseSet] = null


  /**
    * Steps for setup
    * 1 : Remove from the CPIntVar the values that are not present in the MDD
    * 2 : Set active values, edgeCount and activeNodes properly
    * 3 : Set propagate when domain change
    * 4 : Propagate
    */
  override def setup(l: _root_.oscar.cp.core.CPPropagStrength) = {
    if (nEdges == 0) throw Inconsistency

    activeValues = Array.tabulate(nVariables)(x => new ReversibleSharedSparseSet(reversibleContext, staticDomains(x).last() + 1))
    edgeCount = Array.fill(nVariables)(ReversibleInt(0)(reversibleContext))
    activeNodes = Array.fill(nVariables + 1)(new ReversibleSharedSparseSet(reversibleContext, nNodes))


    /** Step 1 **/
    var i = 0
    var j = 0
    while (i < nVariables) {
      val variable = variables(i)
      val values = variable.toArray
      j = 0
      while (j < values.length) {
        if (!staticDomains(i).contains(values(j))) {
          variable.removeValue(values(j))
        }
        j += 1
      }
      i += 1
    }
    /** Step 2 **/
    i = 0
    while (i < nNodes) {
      activeNodes(nodes(i).layer).insert(nodes(i).id)
      i += 1
    }

    i = 0
    while (i < nEdges) {
      val edge = edges(i)
      activeValues(edge.variableId).insert(edge.value)
      edgeCount(edge.variableId).setValue(edgeCount(edge.variableId).value + 1)
      i += 1
    }

    /** Step 3 **/
    i = 0
    while (i < nVariables) {
      variables(i).callPropagateWhenDomainChanges(this)
      i += 1
    }

    /** Step 4 **/
    val rez = propagate()
    rez
  }

  override def propagate(): Unit = {
    var i = 0
    while (i < nVariables) {
      if (variables(i).size != activeValues(i).size) {
        update(i)
      }
      i += 1
    }
  }


  /**
    * Propagate mechanism :
    * For each variable : find the set of values removed in domain and not removed in mdd (thanks to current domain in mdd)
    * Easy to find number of edges (for each value, add support size)
    * Then, use either reset or normal method
    * Keep 2 lists : InvalidatedNodes and keptNodes (bottom and top).
    * Based on those 2 key lists, can propagate
    **/

  /**
    * The propagation inside the mdd :
    * I have the invalidatedNodes list
    * => Can easily know how many edges will be deleted (in/out edge size for each node)
    * Then, classical method or reset and do it again ;)
    **/

  private def update(layer: Int): Unit = {
    var i = 0
    var j = 0

    /** Step 1 : count the number of edges that will be removed and remember every value removed (in array) **/
    var countUp = 0
    var countDown = 0
    var numEdgesRemoved = 0
    val nValuesRemoved = activeValues(layer).size - variables(layer).size
    val removedValues = Array.ofDim[Int](nValuesRemoved)
    i = 0
    j = 0
    while (i < activeValues(layer).size) {
      val value: Int = activeValues(layer)(i)
      if (!variables(layer).hasValue(value)) {
        removedValues(j) = value
        numEdgesRemoved += supports(layer)(value).size
        j += 1
      }
      i += 1
    }

    var invalidatedListTop = new util.ArrayList[Int]()
    var invalidatedListBottom = new util.ArrayList[Int]()
    /** Step 2 : Delete the edges **/

    /** No reset on the layer **/
    if (2 * numEdgesRemoved < edgeCount(layer).value) {
      // Remove each edge -> If edge unvalidate a node, add it to the invalidated list (top or bottom)
      i = 0
      while (i < nValuesRemoved) {
        /** For each value that needs to be removed in the mdd on layer **/
        val value = removedValues(i)
        activeValues(layer).remove(value)
        /** Suppress all the edges in the support set **/
        val edgesSet = supports(layer)(value)
        if (edgesSet != null) {
          j = 0
          while (j < edgesSet.size) {
            val edgeId = edgesSet(j)
            val edge = edges(edgeId)
            val topNode = nodes(edge.topNode)
            val botNode = nodes(edge.bottomNode)
            /** If top node has no more ouEdges **/
            topNode.removeOutEdge(edgeId)
            if (topNode.sizeOutEdges == 0) {
              invalidatedListTop.add(topNode.id)
              countUp += topNode.sizeInEdges
              if (activeNodes(layer).hasValue(topNode.id)) activeNodes(layer).remove(topNode.id)
              else println("aaaaah 1")
            }
            /** If bot node has no more inEdges **/
            botNode.removeInEdge(edgeId)
            if (botNode.sizeInEdges == 0) {
              invalidatedListBottom.add(botNode.id)
              countDown += botNode.sizeOutEdges
              if (activeNodes(layer + 1).hasValue(botNode.id)) activeNodes(layer + 1).remove(botNode.id)
              else println("aaaaah 2")
            }
            j += 1
          }
          edgesSet.clear()
        }
        else {
          println("We got a problem, no support created while it is needed")
        }
        i += 1
      }
    }

    /** Reset the layer **/
    else {
      // Reset the layer -> remember number of valid nodes and reset. Then re-add all edges and reactivate nodes. The invalidated nodes can be obtained easily with the previous number of valid nodes
      val numValidTop = activeNodes(layer).size
      val numValidBot = activeNodes(layer + 1).size
      // Reset all the nodes
      i = 0
      /**
        * For each value to remove : clean support (maybe not needed ? ) and remove the activeValue
        */
      while (i < nValuesRemoved) {
        val value = removedValues(i)
        val sup = supports(layer)(value)
        // TODO : sup.clear is useless ?
        sup.clear()
        activeValues(layer).remove(value)
        i += 1
      }

      /**
        * For each active node on layer and layer+1 : clear bot edges and top edges
        */
      i = 0
      while (i < activeNodes(layer).size) {
        nodes(activeNodes(layer)(i)).clearOutEdges()
        i += 1
      }
      i = 0
      while (i < activeNodes(layer + 1).size) {
        nodes(activeNodes(layer + 1)(i)).clearInEdges()
        i += 1
      }

      // Reset bot layers
      activeNodes(layer).clear()
      activeNodes(layer + 1).clear()

      /** Re-add the edges based on the support still present **/
      i = 0
      while (i < activeValues(layer).size) {
        val value = activeValues(layer)(i)

        val sup = supports(layer)(value)
        j = 0
        while (j < sup.size) {
          val edgeId = sup(j)
          val edge = edges(edgeId)
          val topNode = nodes(edge.topNode)
          val botNode = nodes(edge.bottomNode)

          topNode.reAddOutEdge(edgeId)
          botNode.reAddInEdge(edgeId)

          // Re-insert top and bot as valid if necessary
          if (topNode.sizeOutEdges == 1) {
            activeNodes(topNode.layer).restore(topNode.id)
          }
          if (botNode.sizeInEdges == 1) {
            activeNodes(botNode.layer).restore(botNode.id)
          }

          j += 1
        }
        i += 1

      }
      /** At the end, need to do the invalidated node **/
      i = activeNodes(layer).size
      while (i < numValidTop) {
        invalidatedListTop.add(activeNodes(layer)(i))
        countUp += nodes(activeNodes(layer)(i)).sizeInEdges
        i += 1
      }

      i = activeNodes(layer + 1).size
      while (i < numValidBot) {
        invalidatedListBottom.add(activeNodes(layer + 1)(i))
        countDown += nodes(activeNodes(layer + 1)(i)).sizeOutEdges
        i += 1
      }
    }

    // Update number of edges on layer
    edgeCount(layer).setValue(edgeCount(layer) - numEdgesRemoved)


    /** Step 3 : From layer to 0 : prune **/
    // First do it in a classical way, I will improve it afterwards

    var curLayer = layer - 1
    while (countUp > 0) {
      /** Regular removal : edge after edge **/
      if (countUp < edgeCount(curLayer).value / 2) {
        edgeCount(curLayer).setValue(edgeCount(curLayer) - countUp)
        countUp = 0
        val newInvalitedListTop = new util.ArrayList[Int]()

        /** For each node of invalidated node, remove all its edges **/
        i = 0
        while (i < invalidatedListTop.size()) {
          val nodeId = invalidatedListTop.get(i)
          val node = nodes(nodeId)
          j = 0
          while (j < node.sizeInEdges) {
            val edgeId = node.getIn(j)
            val edge = edges(edgeId)
            val sup = supports(edge.variableId)(edge.value)

            /** Delete the value, update support and if support empty, remove in domain and in active values **/
            sup.remove(edgeId)
            if (sup.size == 0) {
              variables(edge.variableId).removeValue(edge.value)
              activeValues(edge.variableId).remove(edge.value)
            }

            /** Delete the edge in the mdd and if invalited node is created, handle it **/
            val topNode = nodes(edge.topNode)
            topNode.removeOutEdge(edgeId)

            if (topNode.sizeOutEdges == 0) {
              newInvalitedListTop.add(topNode.id)
              if (activeNodes(topNode.layer).hasValue(topNode.id)) activeNodes(topNode.layer).remove(topNode.id)
              else println("aaaaaaah 5")
              countUp += topNode.sizeInEdges
            }
            j += 1
          }
          node.clearInEdges()
          i += 1
        }
        invalidatedListTop = newInvalitedListTop
      }

      /** Reset removal **/
      else {
        edgeCount(curLayer).setValue(edgeCount(curLayer) - countUp)
        countUp = if (curLayer == 0) 0 else edgeCount(curLayer - 1).value
        // We will lower the number of edges removed when we put back a node as valid

        val previousActiveValues = activeValues(curLayer).size
        val previousActiveNodes = activeNodes(curLayer).size
        /**
          * First, reset the top nodes
          */
        i = 0
        while (i < activeNodes(curLayer).size) {
          nodes(activeNodes(curLayer)(i)).clearOutEdges()
          i += 1
        }
        activeNodes(curLayer).clear()

        /**
          * Then, for every activated edge, clear support and activate values
          */
        i = 0
        while (i < activeValues(curLayer).size) {
          val value = activeValues(curLayer)(i)
          supports(curLayer)(value).clear()
          i += 1
        }
        activeValues(curLayer).clear()

        /**
          * Now that everything is cleared, re-add the edges of layer under
          */
        i = 0
        while (i < activeNodes(curLayer + 1).size) {
          val nodeId = activeNodes(curLayer + 1)(i)
          val node = nodes(nodeId)

          j = 0
          while (j < node.sizeInEdges) {
            val edgeId = node.getIn(j)
            val edge = edges(edgeId)

            val topNode = nodes(edge.topNode)
            topNode.reAddOutEdge(edgeId)
            if (topNode.sizeOutEdges == 1) {
              // Re-activate the node
              activeNodes(curLayer).restore(topNode.id)
              countUp -= topNode.sizeInEdges
            }
            supports(curLayer)(edge.value).restore(edgeId)
            if (supports(curLayer)(edge.value).size == 1) {
              // Re-activate the value
              activeValues(curLayer).restore(edge.value)
            }

            j += 1
          }
          i += 1
        }

        /** Prune domain is necessary **/
        i = activeValues(curLayer).size
        while (i < previousActiveValues) {
          variables(curLayer).removeValue(activeValues(curLayer)(i))
          i += 1
        }

        /** Update the list of top Node **/
        invalidatedListTop.clear()
        i = activeNodes(curLayer).size
        while (i < previousActiveNodes) {
          invalidatedListTop.add(activeNodes(curLayer)(i))
          i += 1
        }
      }
      curLayer -= 1
    }


    /** Step 4 : From layer to arity : prune **/
    curLayer = layer + 1
    while (countDown > 0) {
      /** Regular removal : edge after edge **/
      if (countDown < edgeCount(curLayer).value / 2) {
        edgeCount(curLayer).setValue(edgeCount(curLayer) - countDown)
        countDown = 0
        val newInvalitedListBottom = new util.ArrayList[Int]()

        /** For each node of invalidated node, remove all its edges **/
        i = 0
        while (i < invalidatedListBottom.size()) {
          val nodeId = invalidatedListBottom.get(i)
          val node = nodes(nodeId)
          j = 0
          while (j < node.sizeOutEdges) {
            val edgeId = node.getOut(j)
            val edge = edges(edgeId)
            val sup = supports(edge.variableId)(edge.value)

            /** Delete the value, update support and if support empty, remove in domain and in active values **/
            sup.remove(edgeId)
            if (sup.size == 0) {
              variables(edge.variableId).removeValue(edge.value)
              activeValues(edge.variableId).remove(edge.value)
            }

            /** Delete the edge in the mdd and if invalited node is created, handle it **/
            val botNode = nodes(edge.bottomNode)
            botNode.removeInEdge(edgeId)

            if (botNode.sizeInEdges == 0) {
              newInvalitedListBottom.add(botNode.id)
              if (activeNodes(botNode.layer).hasValue(botNode.id)) activeNodes(botNode.layer).remove(botNode.id)
              else println("aaaaaaah 6")
              countDown += botNode.sizeOutEdges
            }
            j += 1
          }
          node.clearOutEdges()
          i += 1
        }
        invalidatedListBottom = newInvalitedListBottom
      }

      /** Reset removal **/
      else {
        edgeCount(curLayer).setValue(edgeCount(curLayer) - countDown)
        countDown = if (curLayer == nVariables - 1) 0 else edgeCount(curLayer + 1).value
        // We will lower the number of edges removed when we put back a node as valid

        val previousActiveValues = activeValues(curLayer).size
        val previousActiveNodes = activeNodes(curLayer + 1).size
        /**
          * First, reset the bottom nodes
          */
        i = 0
        while (i < activeNodes(curLayer + 1).size) {
          nodes(activeNodes(curLayer + 1)(i)).clearInEdges()
          i += 1
        }
        activeNodes(curLayer + 1).clear()

        /**
          * Then, for every activated edge, clear support and activate values
          */
        i = 0
        while (i < activeValues(curLayer).size) {
          val value = activeValues(curLayer)(i)
          supports(curLayer)(value).clear()
          i += 1
        }
        activeValues(curLayer).clear()

        /**
          * Now that everything is cleared, re-add the edges of layer under
          */
        i = 0
        while (i < activeNodes(curLayer).size) {
          val nodeId = activeNodes(curLayer)(i)
          val node = nodes(nodeId)

          j = 0
          while (j < node.sizeOutEdges) {
            val edgeId = node.getOut(j)
            val edge = edges(edgeId)

            val botNode = nodes(edge.bottomNode)
            botNode.reAddInEdge(edgeId)
            if (botNode.sizeInEdges == 1) {
              // Re-activate the node
              activeNodes(curLayer + 1).restore(botNode.id)
              countDown -= botNode.sizeOutEdges
            }

            supports(curLayer)(edge.value).restore(edgeId)
            if (supports(curLayer)(edge.value).size == 1) {
              // Re-activate the value
              activeValues(curLayer).restore(edge.value)
            }
            j += 1
          }
          i += 1
        }

        /** Prune domain is necessary **/
        i = activeValues(curLayer).size
        while (i < previousActiveValues) {
          variables(curLayer).removeValue(activeValues(curLayer)(i))
          i += 1
        }

        /** Update the list of top Node **/
        invalidatedListBottom.clear()
        i = activeNodes(curLayer + 1).size
        while (i < previousActiveNodes) {
          invalidatedListBottom.add(activeNodes(curLayer + 1)(i))
          i += 1
        }
      }
      curLayer += 1
    }
  }

  private def setSupports(): Array[Array[ReversibleSharedSparseSet]] = {
    //val outputSupportSet = Array.fill[util.HashMap[Int, ReversibleSharedSparseSet]](nVariables)(new util.HashMap[Int, ReversibleSharedSparseSet]())

    val nValues = if (nEdges == 0) 0 else staticDomains.map(_.last()).max + 1

    val outputSupportSet = Array.ofDim[ReversibleSharedSparseSet](nVariables,nValues)

    for (i <- 0 until nEdges) {
      val edge = edges(i)
      var sup = outputSupportSet(edge.variableId)(edge.value)
      if (sup == null) {
        sup = new ReversibleSharedSparseSet(reversibleContext, nEdges)
        outputSupportSet(edge.variableId)(edge.value) =  sup
      }
      sup.insert(edge.id)
    }
    outputSupportSet
  }

  def associatedVars(): Iterable[CPVar] = variables



}