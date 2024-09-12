package oscar.cp.core.watcher

import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint

class WatcherListL2(store: CPStore) {
  
  private[this] var lastMagic = -1L
  private[this] var watchers: Array[Watcher] = new Array[Watcher](4)
  private[this] var index: Int = 0

  @inline final def length: Int = index
  
  @inline final def isEmpty = index == 0
  
  final def register(constraint: Constraint, cond: => Boolean): Unit = {
    val watcher = new WatcherL2Garded(constraint, cond)
    if (index == watchers.length) growStack()
    watchers(index) = watcher
    trail()
    index += 1
  }

  final def register(constraint: Constraint): Unit = {
    val watcher = new WatcherL2(constraint)
    if (index == watchers.length) growStack()
    watchers(index) = watcher
    trail()
    index += 1
  }
  
  final def register(watcher: Watcher): Unit = {
    if (index == watchers.length) growStack()
    watchers(index) = watcher
    trail()
    index += 1
  }
  
  @inline final def clear(): Unit = {
    trail()
    index = 0
  }
  
  @inline final def enqueue(): Unit = {
    var i = index
    while (i > 0) { 
      i -= 1
      watchers(i).awake()
    }
  }  
  
  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      val newIndex = index
      store.trail(new TrailEntry { final override def restore(): Unit = index = newIndex })
    }
  }
  
  // Double the size of the stack
  @inline private def growStack(): Unit = {
    val newWatchers = new Array[Watcher](watchers.length * 2)
    System.arraycopy(watchers, 0, newWatchers, 0, watchers.length)
    watchers = newWatchers   
  }
}