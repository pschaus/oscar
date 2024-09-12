package oscar.cp.constraints

import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPVar}
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.algo.reversible.ReversibleBoolean
import oscar.cp.core.watcher.Watcher
import oscar.algo.SortUtils

/** @author Renaud Hartert ren.hartert@gmail.com */

final class WatcherKnapsack(knapsack: Constraint, boolVar: CPBoolVar, weight: Int, requiredLoad: ReversibleInt, possibleLoad: ReversibleInt) extends Watcher {
  private[this] val store = boolVar.store
  final override def awake(): Unit = {
    if (!knapsack.inPropagate()) {
      if (boolVar.isTrue) requiredLoad += weight
      else possibleLoad -= weight
      store.enqueueL2(knapsack)
    }
  }
}

final class LightBinaryKnapsack(items: Array[CPBoolVar], weights: Array[Int], load: CPIntVar) extends Constraint(load.store, "LightBinaryKnapsack") {

  override def associatedVars(): Iterable[CPVar] = items ++ Array(load)

  idempotent = true

  private[this] val nItems = items.length
  private[this] val sortedItems = Array.tabulate(nItems)(i => i)
  private[this] val largestItemRev = new ReversibleInt(s, 0)
  private[this] var largestItem = 0

  private[this] val requiredLoadRev = new ReversibleInt(s, 0)
  private[this] val possibleLoadRev = new ReversibleInt(s, 0)
  private[this] var requiredLoad = 0
  private[this] var possibleLoad = 0

  final override def setup(l: CPPropagStrength): Unit = {
    init()
    if(isActive) {
      var i = nItems
      while (i > 0) {
        i -= 1
        val watcher = new WatcherKnapsack(this, items(i), weights(i), requiredLoadRev, possibleLoadRev)
        items(i).awakeOnChanges(watcher)
      }
      load.callPropagateWhenDomainChanges(this)
    }
  }

  @inline private def init(): Unit = {
    // Reset structures
    requiredLoad = 0
    possibleLoad = 0
    // Sort items
    SortUtils.mergeSort(sortedItems, weights)
    // Compute loads
    var i = nItems
    while (i > 0) {
      i -= 1
      val item = items(i)
      if (!item.isBound) possibleLoad += weights(i)
      else if (item.isTrue) {
        val weight = weights(i)
        requiredLoad += weight
        possibleLoad += weight
      }
    }
    // Largest 
    largestItem = nItems - 1
    while (largestItem >= 0 && items(sortedItems(largestItem)).isBound) largestItem -= 1
    // Initial filtering
    filterItems()
    largestItemRev.value = largestItem
    requiredLoadRev.value = requiredLoad
    possibleLoadRev.value = possibleLoad
  }

  final override def propagate(): Unit = {
    // Cache
    largestItem = largestItemRev.value
    requiredLoad = requiredLoadRev.value
    possibleLoad = possibleLoadRev.value
    // Filtering
    filterItems()
    // Trail
    largestItemRev.value = largestItem
    requiredLoadRev.value = requiredLoad
    possibleLoadRev.value = possibleLoad
  }

  @inline private def filterItems(): Unit = {
    load.updateMax(possibleLoad)
    load.updateMin(requiredLoad)
    var maxWeight = load.max - requiredLoad
    var minWeight = possibleLoad - load.min
    var continue = largestItem >= 0
    while (continue && largestItem >= 0) {
      val itemId = sortedItems(largestItem)
      val item = items(itemId)
      if (item.isBound) largestItem -= 1
      else {
        val weight = weights(itemId)
        if (weight > maxWeight) {
          item.assignFalse()
          possibleLoad -= weight
          load.updateMax(possibleLoad)
          largestItem -= 1
          minWeight -= weight
          maxWeight = load.max - requiredLoad
        }
        else if (minWeight < weight) {
          item.assignTrue()
          requiredLoad += weight
          load.updateMin(requiredLoad)
          largestItem -= 1
          minWeight = possibleLoad - load.min
          maxWeight -= weight
        }
        else
          continue = false
      }
    }
  }
}