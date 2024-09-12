/** *****************************************************************************
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
 * **************************************************************************** */

package oscar.cp.constraints.diagrams

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleSparseBitSet, ReversibleSparseSetSimple}
import oscar.cp.constraints.tables._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation the various layers that are supported by the Compact Diagram algorithm (CD)
 * for the diagram constraint and its extension to the basic smart diagrams constraint
 *
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *
 *         Reference(s) :
 *  - Compact-MDD: Efficiently Filtering (s)MDD Constraints with Reversible Sparse Bit-sets, Helene Verhaeghe, Christophe Lecoutre, Pierre Schaus, IJCAI18
 *  - Extending Compact-Diagram to Basic Smart Multi-Valued Variable Diagrams, Helene Verhaeghe, Christophe Lecoutre, Pierre Schaus, CPAIOR19
 */

/**
 * Create a layer object containing all the reversible sparse bit sets
 * associated to one layer of a diagram
 */
object LayeredReversibleSparseBitSet {

  /**
   * Connect an edge to the tail and head node
   *
   * @param bitset The layer
   * @param idUp   the id of the tail node
   * @param idDown the id of the head node
   * @param idx    the id of the edge
   */
  @inline private def addNodes(bitset: LayeredReversibleSparseBitSet, idUp: Int, idDown: Int, idx: Int) = {
    bitset.supportsUpNode(idUp).set(idx)
    bitset.supportsDownNode(idDown).set(idx)
  }

  /**
   * Create the layer with ground labels only
   *
   * @param lvl
   * @param x
   * @param upNode
   * @param downNode
   * @param edges
   * @return
   */
  def apply1(lvl: Int, x: CPIntVar, upNode: ReversibleSparseSetSimple, downNode: ReversibleSparseSetSimple, edges: Array[(Int, Int, Int)]) = {
    val LRSBS = new LayeredReversibleSparseBitSet(lvl, x, upNode, downNode, (edges.size + 63) / 64, edges.indices)
    val buffer = Array.fill(LRSBS.supports.length)(new ArrayBuffer[Int]())
    for (i <- edges.indices) {
      buffer(edges(i)._2) += i
      addNodes(LRSBS, edges(i)._1, edges(i)._3, i)
    }
    for (i <- buffer.indices) {
      if (buffer(i).nonEmpty) {
        LRSBS.supports(i) = new LRSBS.BitSet(buffer(i))
      } else {
        x.removeValue(i)
      }
    }
    LRSBS
  }

  /**
   * Create the appropriate layer with Basic Smart elements as label
   *
   * @param lvl
   * @param x
   * @param upNode
   * @param downNode
   * @param edges
   * @return
   */
  def apply2(lvl: Int, x: CPIntVar, upNode: ReversibleSparseSetSimple, downNode: ReversibleSparseSetSimple, edges: Array[(Int, BasicSmartElement, Int)]): LayeredReversibleSparseBitSet = {
    val buffers = Array.fill(4)(new scala.collection.mutable.ArrayBuffer[Int]())
    // group each type of basic smart element by category
    for (i <- edges.indices) {
      edges(i)._2 match {
        case _: Equal => buffers(0) += i
        case _: Star => buffers(0) += i
        case _: NotEqual => buffers(0) += i
        case _: LessEq => buffers(1) += i
        case _: GreatEq => buffers(2) += i
        case _ => buffers(3) += i
      }
    }
    // compute nb of words of each category
    val nbWords = buffers.map(b => (b.size + 63) / 64)
    val nbWordTotal = nbWords.sum

    // create the buffers for filling the supports bitsets for easy access to arcs of a given label
    val span = 0 to x.max
    val bufferSupport = Array.fill(span.length)(new ArrayBuffer[Int]())
    val bufferSupportAux = Array.fill(span.length)(new ArrayBuffer[Int]())

    // generic method to fill the bitsets given a category id, a word offset and
    // a fct giving how to fill the bitset given a basic smart element
    def fill(id: Int, bitset: LayeredReversibleSparseBitSet, offset: Int, fct: (BasicSmartElement, Int) => Unit) = {
      val off = offset * 64
      var idx = buffers(id).length
      while (idx > 0) {
        idx -= 1
        val off_idx = off + idx
        val e = edges(buffers(id)(idx))
        fct(e._2, off_idx)
        addNodes(bitset, e._1, e._3, off_idx)
      }
    }

    // fill the category =, !=, *
    def fillEqStarNeq(bitset: LayeredReversibleSparseBitSet): Unit = fill(0, bitset, 0, (bse, off_idx) => bse match {
      case Equal(v) => bufferSupport(v) += off_idx; bufferSupportAux(v) += off_idx
      case Star() => span.foreach(v => bufferSupport(v) += off_idx)
      case NotEqual(v) => span.foreach(w => if (v != w) bufferSupport(w) += off_idx)
    })

    // fill the category <=
    def fillLeq(bitset: LayeredReversibleSparseBitSet, offset: Int): Unit = fill(1, bitset, offset, (bse, off_idx) => bse match {
      case LessEq(v) => span.foreach(w => if (w <= v) bufferSupport(w) += off_idx else bufferSupportAux(w) += off_idx)
    })

    // fill the category >=
    def fillGeq(bitset: LayeredReversibleSparseBitSet, offset: Int): Unit = fill(2, bitset, offset, (bse, off_idx) => bse match {
      case GreatEq(v) => span.foreach(w => if (w >= v) bufferSupport(w) += off_idx else bufferSupportAux(w) += off_idx)
    })

    // fill the category \in S
    def fillSet(bitset: LayeredReversibleSparseBitSet, offset: Int): Unit = fill(3, bitset, offset, (bse, off_idx) =>
      bse.foreach(x, v => bufferSupport(v) += off_idx))

    // Create the right layer depending on the represented categories
    val LRSBS = if (buffers(1).isEmpty && buffers(2).isEmpty && buffers(3).isEmpty) {
      // only =v, * and !=v
      val LRSBS = new LayeredReversibleSparseBitSetEqStarNeq(lvl, x, upNode, downNode, nbWordTotal, buffers(0).indices)
      fillEqStarNeq(LRSBS)
      LRSBS
    } else if (buffers(1).isEmpty && buffers(2).isEmpty) {
      // only =v, * and !=v
      val offset = nbWords(0)
      val initValues = buffers(0).indices ++ buffers(3).indices.map(_ + 64 * offset)
      val LRSBS = new LayeredReversibleSparseBitSetEqStarNeqSet(lvl, x, upNode, downNode, nbWordTotal, initValues, offset)
      fillEqStarNeq(LRSBS)
      fillSet(LRSBS, offset)
      LRSBS
    } else if (buffers(0).isEmpty && buffers(3).isEmpty) {
      // only <=v, >=v
      val offset = nbWords(1)
      val initValues = buffers(1).indices ++ buffers(2).indices.map(_ + 64 * offset)
      val LRSBS = new LayeredReversibleSparseBitSetIneq(lvl, x, upNode, downNode, nbWordTotal, initValues, offset)
      fillLeq(LRSBS, 0)
      fillGeq(LRSBS, offset)
      LRSBS
    } else if (buffers(3).isEmpty) {
      // only =v, *, !=v, <=v, >=v
      val offsetMid = nbWords(0)
      val offsetIneq = offsetMid + nbWords(1)
      val initValues = buffers(0).indices ++ buffers(1).indices.map(_ + 64 * offsetMid) ++ buffers(2).indices.map(_ + 64 * offsetIneq)
      val LRSBS = new LayeredReversibleSparseBitSetEqStarNeqIneq(lvl, x, upNode, downNode, nbWordTotal, initValues, offsetMid, offsetIneq)
      fillEqStarNeq(LRSBS)
      fillLeq(LRSBS, offsetMid)
      fillGeq(LRSBS, offsetIneq)
      LRSBS
    } else {
      // all
      val offsetSet = nbWords(0)
      val offsetMid = offsetSet + nbWords(3)
      val offsetIneq = offsetMid + nbWords(1)
      val initValues = buffers(0).indices ++ buffers(3).indices.map(_ + 64 * offsetSet) ++ buffers(1).indices.map(_ + 64 * offsetMid) ++ buffers(2).indices.map(_ + 64 * offsetIneq)
      val LRSBS = new LayeredReversibleSparseBitSetAll(lvl, x, upNode, downNode, nbWordTotal, initValues, offsetMid, offsetSet, offsetIneq)
      fillEqStarNeq(LRSBS)
      fillSet(LRSBS, offsetSet)
      fillLeq(LRSBS, offsetMid)
      fillGeq(LRSBS, offsetIneq)
      LRSBS
    }
    // Fill the supports and supportsAux of the layer with the content of the buffers
    var i = bufferSupport.length
    while (i > 0) {
      i -= 1
      if (bufferSupport(i).nonEmpty)
        LRSBS.supports(i) = new LRSBS.BitSet(bufferSupport(i))
      else
        x.removeValue(i)
    }
    for (i <- bufferSupportAux.indices) {
      LRSBS.supportsAux(i) = new LRSBS.BitSet(bufferSupportAux(i))
    }
    LRSBS
  }
}

/**
 * Object representing one layer (with ground label only)
 *
 * @param lvl           id of the lvl
 * @param x             associated var
 * @param upNode        reversible sparse set containing the active nodes above the layer
 * @param downNode      reversible sparse set containing the active nodes below the layer
 * @param nbWords       nb of words
 * @param initialValues the initial values contained in the set
 */
class LayeredReversibleSparseBitSet(
                                     val lvl: Int,
                                     val x: CPIntVar,
                                     val upNode: ReversibleSparseSetSimple,
                                     val downNode: ReversibleSparseSetSimple,
                                     val nbWords: Int,
                                     initialValues: Iterable[Int]
                                   )
  extends ReversibleSparseBitSet(x.store, nbWords * 64, initialValues) {

  protected[LayeredReversibleSparseBitSet] val nbUpNode: Int = upNode.size
  protected[LayeredReversibleSparseBitSet] val nbDownNode: Int = downNode.size
  protected[LayeredReversibleSparseBitSet] val span: Int = x.max + 1 // assuming variables have been offseted before
  val supports: Array[BitSet] = new Array[BitSet](span)
//  protected[LayeredReversibleSparseBitSet] val supports: Array[BitSet] = new Array[BitSet](span)

  protected[LayeredReversibleSparseBitSet] val supportsUpNode: Array[BitSet] = Array.fill(nbUpNode)(new BitSet(List()))
  protected[LayeredReversibleSparseBitSet] val supportsDownNode: Array[BitSet] = Array.fill(nbDownNode)(new BitSet(List()))

  protected[LayeredReversibleSparseBitSet] var hasChanged = false
  protected[LayeredReversibleSparseBitSet] val domainArray = new Array[Int](span)
  protected[LayeredReversibleSparseBitSet] var domainArraySize = 0

  protected[LayeredReversibleSparseBitSet] var levelUp: LayeredReversibleSparseBitSet = null
  protected[LayeredReversibleSparseBitSet] var levelDown: LayeredReversibleSparseBitSet = null
  protected[LayeredReversibleSparseBitSet] var delta: DeltaIntVar = null

  // connect with level up
  def putLevelUp(level: LayeredReversibleSparseBitSet) =
    this.levelUp = level

  // connect with level down
  def putLevelDown(level: LayeredReversibleSparseBitSet) =
    this.levelDown = level

  def putDelta(delta: DeltaIntVar) =
    this.delta = delta

  // clear the collections on all layers
  def propagateInit(last: Int): Unit = {
    clearCollected()
    hasChanged = false
    if (levelDown != null && lvl < last)
      levelDown.propagateInit(last)
  }

  // collect edges from a node up the layer
  def collectUp(id: Int) =
    collect(supportsUpNode(id))

  // collect edges from a node down the layer
  def collectDown(id: Int) =
    collect(supportsDownNode(id))

  // update the reversible sparse bit set
  def update = {
    val varSize = x.size
    if (varSize == 1) {
      collect(supports(x.min))
      reverseCollected()
    } else {
      if (delta.size <= varSize) {
        // incremental update
        domainArraySize = delta.fillArray(domainArray)
        while (domainArraySize > 0) {
          domainArraySize -= 1
          collect(supports(domainArray(domainArraySize)))
        }
      } else {
        // reset update
        domainArraySize = x.fillArray(domainArray)
        while (domainArraySize > 0) {
          domainArraySize -= 1
          collect(supports(domainArray(domainArraySize)))
        }
        reverseCollected()
      }
    }
    hasChanged = true
  }

  def initUpdate = {
    val varSize = x.size
    if (varSize == 1) {
      collect(supports(x.min))
      reverseCollected()
    } else {
        // reset update
        domainArraySize = x.fillArray(domainArray)
        while (domainArraySize > 0) {
          domainArraySize -= 1
          collect(supports(domainArray(domainArraySize)))
        }
        reverseCollected()
    }
    hasChanged = true
  }

  // remove the collection
  def removeCollected2(): Unit = {
    if (context.magic != timeStamp) {
      trail()
      timeStamp = context.magic
    }
    var i = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      words(offset) = words(offset) & ~tempMask(offset)
      if (words(offset) == 0L) {
        nNonZero -= 1
        nonZeroIdx(i) = nonZeroIdx(nNonZero)
        nonZeroIdx(nNonZero) = offset
      }
    }
  }

  // verify the validity of the remaining values of a domain
  def propagate = {
    domainArraySize = x.fillArray(domainArray)
    var value = 0
    while (domainArraySize > 0) {
      domainArraySize -= 1
      value = domainArray(domainArraySize)
      if (!intersect(supports(value)))
        x.removeValue(value)
    }
  }

  // top-down propagation of one layer to another, detection of inactive layers
  def propagateDownSkip(first: Int, last: Int): Unit = {
    if (hasChanged) {
      propagateDown(first, last)
    } else {
      if (levelDown != null && lvl < last)
        levelDown.propagateDownSkip(first, last)
      else
        propagateUpInit(first)
    }
  }

  // top-down propagation of one layer to another, active propagation
  def propagateDown(first: Int, last: Int): Unit = {
    if (levelDown != null && lvl < last) {
      removeCollected2()
      if (isEmpty())
        throw Inconsistency
      var localChange = false
      downNode.filter(i =>
        if (!intersect(supportsDownNode(i))) {
          localChange = true
          levelDown.collectUp(i)
          false
        } else {
          true
        }
      )
      if (localChange) {
        levelDown.propagateDown(first, last)
      } else {
        levelDown.propagateDownSkip(first, last)
      }
    } else {
      removeCollected2()
      if (isEmpty())
        throw Inconsistency
      propagateUpInit(first)
    }
  }

  // bottom-up propagation of one layer to another, detection of inactive layers
  def propagateUpSkip(first: Int): Unit = {
    if (hasChanged) {
      propagateUp(first)
    } else if (levelUp != null && lvl > first) {
      levelUp.propagateUpSkip(first)
    }
  }

  // bottom-up propagation (first after top-down) of one layer to another, active propagation
  def propagateUpInit(first: Int): Unit = {
    if (lvl > first) {
      if (levelUp != null) {
        var localChange = false
        if (hasChanged) {
          upNode.filter(i =>
            if (!intersect(supportsUpNode(i))) {
              localChange = true
              levelUp.collectDown(i)
              false
            } else {
              true
            }
          )
        }
        if (localChange)
          levelUp.propagateUp(first)
        else
          levelUp.propagateUpSkip(first)
      }
    } else {
      removeCollected2()
      if (isEmpty())
        throw Inconsistency
    }
  }

  // bottom-up propagation of one layer to another, active propagation
  def propagateUp(first: Int): Unit = {
    if (levelUp != null && lvl > first) {
      removeCollected2()
      if (isEmpty())
        throw Inconsistency
      var localChange = false
      upNode.filter(i =>
        if (!intersect(supportsUpNode(i))) {
          localChange = true
          levelUp.collectDown(i)
          false
        } else {
          true
        }
      )
      if (localChange)
        levelUp.propagateUp(first)
      else
        levelUp.propagateUpSkip(first)
    } else {
      removeCollected2()
      if (isEmpty())
        throw Inconsistency
    }
  }

}

/**
 * Object representing one layer (with basic smart label only)
 *
 * @param lvl           id of the lvl
 * @param x             associated var
 * @param upNode        reversible sparse set containing the active nodes above the layer
 * @param downNode      reversible sparse set containing the active nodes below the layer
 * @param nbWords       nb of words
 * @param initialValues the initial values contained in the set
 */
class LayeredReversibleSparseBitSetBasicSmart(
                                               lvl: Int,
                                               x: CPIntVar,
                                               upNode: ReversibleSparseSetSimple,
                                               downNode: ReversibleSparseSetSimple,
                                               nbWords: Int,
                                               initialValues: Iterable[Int]
                                             )
  extends LayeredReversibleSparseBitSet(lvl, x, upNode, downNode, nbWords, initialValues) {

  // second set of support bitset to handle the smart elements
  val supportsAux: Array[BitSet] = new Array[BitSet](span)
}

// Layer when there is only =, !=, *
class LayeredReversibleSparseBitSetEqStarNeq(
                                              lvl: Int,
                                              x: CPIntVar,
                                              upNode: ReversibleSparseSetSimple,
                                              downNode: ReversibleSparseSetSimple,
                                              nbWords: Int,
                                              initialValues: Iterable[Int]
                                            )
  extends LayeredReversibleSparseBitSetBasicSmart(lvl, x, upNode, downNode, nbWords, initialValues) {

  override def update = {
    val varSize = x.size
    if (varSize == 1) {
      collect(supports(x.min))
      reverseCollected()
    } else {
      if (delta.size <= varSize) {
        domainArraySize = delta.fillArray(domainArray)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          collect(supportsAux(domainArray(j)))
        }
      } else {
        domainArraySize = x.fillArray(domainArray)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          collect(supports(domainArray(j)))
        }
        reverseCollected()
      }
    }
    hasChanged = true
  }

}

// Layer when there is only <=, >=
class LayeredReversibleSparseBitSetIneq(
                                         lvl: Int,
                                         x: CPIntVar,
                                         upNode: ReversibleSparseSetSimple,
                                         downNode: ReversibleSparseSetSimple,
                                         nbWords: Int,
                                         initialValues: Iterable[Int],
                                         val offsetMid: Int
                                       )
  extends LayeredReversibleSparseBitSetBasicSmart(lvl, x, upNode, downNode, nbWords, initialValues) {

  @inline private def bothChanged(supmin: Array[Long], supmax: Array[Long]) = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      tempMask(offset) |= (if (offset < offsetMid) supmin(offset) else supmax(offset))
    }
  }

  @inline private def minChanged(supmin: Array[Long]) = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) tempMask(offset) |= supmin(offset)
    }
  }


  @inline private def maxChanged(supmax: Array[Long]) = {
    var i: Int = nNonZero
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset >= offsetMid) tempMask(offset) |= supmax(offset)
    }
  }

  override def update = {
    val varSize = x.size
    if (varSize == 1) {
      collect(supports(x.min))
      reverseCollected()
    } else {
      if (delta.minChanged) {
        if (delta.maxChanged)
          bothChanged(supportsAux(x.min).words, supportsAux(x.max).words)
        else
          minChanged(supportsAux(x.min).words)
      } else {
        if (delta.maxChanged)
          maxChanged(supportsAux(x.max).words)
      }
    }
    hasChanged = true
  }
}

// Layer when there is only =, !=, *, <=, >=
class LayeredReversibleSparseBitSetEqStarNeqIneq(
                                                  lvl: Int,
                                                  x: CPIntVar,
                                                  upNode: ReversibleSparseSetSimple,
                                                  downNode: ReversibleSparseSetSimple,
                                                  nbWords: Int,
                                                  initialValues: Iterable[Int],
                                                  val offsetMid: Int,
                                                  val offsetIneq: Int
                                                )
  extends LayeredReversibleSparseBitSetBasicSmart(lvl, x, upNode, downNode, nbWords, initialValues) {

  @inline protected def classicalBothChanged(supmin: Array[Long], supmax: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = delta.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supportsAux(domainArray(j)).words(offset)
        }
        tempMask(offset) = t
      } else {
        tempMask(offset) |= (if (offset < offsetIneq) supmin(offset) else supmax(offset))
      }
    }
  }

  @inline protected def resetBothChanged(supmin: Array[Long], supmax: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supports(domainArray(j)).words(offset)
        }
        tempMask(offset) = ~t
      } else {
        tempMask(offset) |= (if (offset < offsetIneq) supmin(offset) else supmax(offset))
      }
    }
  }

  @inline protected def classicalMinChanged(supmin: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = delta.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supportsAux(domainArray(j)).words(offset)
        }
        tempMask(offset) = t
      } else if (offset < offsetIneq) {
        tempMask(offset) |= supmin(offset)
      }
    }
  }

  @inline protected def resetMinChanged(supmin: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supports(domainArray(j)).words(offset)
        }
        tempMask(offset) = ~t
      } else if (offset < offsetIneq) {
        tempMask(offset) |= supmin(offset)
      }
    }
  }

  @inline protected def classicalMaxChanged(supmax: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = delta.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supportsAux(domainArray(j)).words(offset)
        }
        tempMask(offset) = t
      } else if (offset >= offsetIneq) {
        tempMask(offset) |= supmax(offset)
      }
    }
  }

  @inline protected def resetMaxChanged(supmax: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supports(domainArray(j)).words(offset)
        }
        tempMask(offset) = ~t
      } else if (offset >= offsetIneq) {
        tempMask(offset) |= supmax(offset)
      }
    }
  }

  @inline protected def classical() = {
    var i: Int = nNonZero
    domainArraySize = delta.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supportsAux(domainArray(j)).words(offset)
        }
        tempMask(offset) = t
      }
    }
  }

  @inline protected def reset() = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        var j = domainArraySize
        while (j > 0) {
          j -= 1
          t |= supports(domainArray(j)).words(offset)
        }
        tempMask(offset) = ~t
      }
    }
  }


  override def update = {
    val varSize = x.size
    if (varSize == 1) {
      collect(supports(x.min))
      reverseCollected()
    } else {
      var i: Int = nNonZero
      if (delta.size <= varSize) {
        if (delta.minChanged) {
          if (delta.maxChanged)
            classicalBothChanged(supportsAux(x.min).words, supportsAux(x.max).words)
          else
            classicalMinChanged(supportsAux(x.min).words)
        } else {
          if (delta.maxChanged)
            classicalMaxChanged(supportsAux(x.max).words)
          else
            classical()
        }
      } else {
        if (delta.minChanged) {
          if (delta.maxChanged)
            resetBothChanged(supportsAux(x.min).words, supportsAux(x.max).words)
          else
            resetMinChanged(supportsAux(x.min).words)
        } else {
          if (delta.maxChanged)
            resetMaxChanged(supportsAux(x.max).words)
          else
            reset()
        }
      }
    }
    hasChanged = true
  }

}

// Layer when there is every basic smart elements
class LayeredReversibleSparseBitSetAll(
                                        lvl: Int,
                                        x: CPIntVar,
                                        upNode: ReversibleSparseSetSimple,
                                        downNode: ReversibleSparseSetSimple,
                                        nbWords: Int,
                                        initialValues: Iterable[Int],
                                        offsetMid: Int,
                                        val offsetSet: Int,
                                        offsetIneq: Int
                                      )
  extends LayeredReversibleSparseBitSetEqStarNeqIneq(lvl, x, upNode, downNode, nbWords, initialValues, offsetMid, offsetIneq) {

  private[this] val domainArrayBis = new Array[Int](span)
  private[this] var domainArrayBisSize = 0

  @inline override protected def classicalBothChanged(supmin: Array[Long], supmax: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    domainArrayBisSize = delta.fillArray(domainArrayBis)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        if (offset < offsetSet) {
          var j = domainArrayBisSize
          while (j > 0) {
            j -= 1
            t |= supportsAux(domainArrayBis(j)).words(offset)
          }
          tempMask(offset) = t
        } else {
          var j = domainArraySize
          while (j > 0) {
            j -= 1
            t |= supports(domainArray(j)).words(offset)
          }
          tempMask(offset) = ~t
        }
      } else {
        tempMask(offset) |= (if (offset < offsetIneq) supmin(offset) else supmax(offset))
      }
    }
  }

  @inline override protected def classicalMinChanged(supmin: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    domainArrayBisSize = delta.fillArray(domainArrayBis)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        if (offset < offsetSet) {
          var j = domainArrayBisSize
          while (j > 0) {
            j -= 1
            t |= supportsAux(domainArrayBis(j)).words(offset)
          }
          tempMask(offset) = t
        } else {
          var j = domainArraySize
          while (j > 0) {
            j -= 1
            t |= supports(domainArray(j)).words(offset)
          }
          tempMask(offset) = ~t
        }
      } else if (offset < offsetIneq) {
        tempMask(offset) |= supmin(offset)
      }
    }
  }

  @inline override protected def classicalMaxChanged(supmax: Array[Long]) = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    domainArrayBisSize = delta.fillArray(domainArrayBis)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        if (offset < offsetSet) {
          var j = domainArrayBisSize
          while (j > 0) {
            j -= 1
            t |= supportsAux(domainArrayBis(j)).words(offset)
          }
          tempMask(offset) = t
        } else {
          var j = domainArraySize
          while (j > 0) {
            j -= 1
            t |= supports(domainArray(j)).words(offset)
          }
          tempMask(offset) = ~t
        }
      } else if (offset >= offsetIneq) {
        tempMask(offset) |= supmax(offset)
      }
    }
  }

  @inline override protected def classical() = {
    var i: Int = nNonZero
    domainArraySize = x.fillArray(domainArray)
    domainArrayBisSize = delta.fillArray(domainArrayBis)
    while (i > 0) {
      i -= 1
      val offset = nonZeroIdx(i)
      if (offset < offsetMid) {
        var t = tempMask(offset)
        if (offset < offsetSet) {
          var j = domainArrayBisSize
          while (j > 0) {
            j -= 1
            t |= supportsAux(domainArrayBis(j)).words(offset)
          }
          tempMask(offset) = t
        } else {
          var j = domainArraySize
          while (j > 0) {
            j -= 1
            t |= supports(domainArray(j)).words(offset)
          }
          tempMask(offset) = ~t
        }
      }
    }
  }

}

// Layer when there is only =, !=, *, \in S
class LayeredReversibleSparseBitSetEqStarNeqSet(
                                                 lvl: Int,
                                                 x: CPIntVar,
                                                 upNode: ReversibleSparseSetSimple,
                                                 downNode: ReversibleSparseSetSimple,
                                                 nbWords: Int,
                                                 initialValues: Iterable[Int],
                                                 val offsetMid: Int
                                               )
  extends LayeredReversibleSparseBitSetBasicSmart(lvl, x, upNode, downNode, nbWords, initialValues) {

  private[this] val domainArrayBis = new Array[Int](span)
  private[this] var domainArrayBisSize = 0


  override def update = {
    val varSize = x.size
    if (varSize == 1) {
      collect(supports(x.min))
      reverseCollected()
    } else {
      var i: Int = nNonZero
      domainArraySize = x.fillArray(domainArray)
      if (delta.size <= varSize) {
        domainArrayBisSize = delta.fillArray(domainArrayBis)
        while (i > 0) {
          i -= 1
          val offset = nonZeroIdx(i)
          var t = tempMask(offset)
          if (offset < offsetMid) {
            var j = domainArrayBisSize
            while (j > 0) {
              j -= 1
              t |= supportsAux(domainArrayBis(j)).words(offset)
            }
            tempMask(offset) = t
          } else {
            var j = domainArraySize
            while (j > 0) {
              j -= 1
              t |= supports(domainArray(j)).words(offset)
            }
            tempMask(offset) = ~t
          }
        }
      } else {
        while (domainArraySize > 0) {
          domainArraySize -= 1
          collect(supports(domainArray(domainArraySize)))
        }
        reverseCollected()
      }
    }
    hasChanged = true
  }

}
