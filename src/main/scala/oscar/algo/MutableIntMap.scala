package oscar.algo

import scala.collection.mutable
import scala.reflect.ClassTag

class MutableIntMap[V] extends mutable.AbstractMap[Int, V] {
  protected var treeSize: Int = 0
  protected var tree: Array[Int] = _

  protected var _size: Int = 0
  protected var _values: Array[Any] = _

  protected val availableTreeNode = mutable.Stack[Int]()
  protected val availableValuePos = mutable.Stack[Int]()

  protected val pathBuffer = new Array[Int](32)

  clear()

  protected def convertKey(key: Int): Int = key ^ (1 << 31)

  class MapEntry(val key: Int, val contentIdx: Int) {
    var value: Any = null

    def getKey: Int = key

    def getValue: V = {
      if (value == null) _values(contentIdx).asInstanceOf[V]
      else value.asInstanceOf[V]
    }

    def setValue(v: V): V = {
      val old = _values(contentIdx).asInstanceOf[V]
      _values(contentIdx) = v
      old
    }

    private[MutableIntMap] def unbind(): Unit = {
      value = _values(contentIdx)
    }

    override def equals(o: Any): Boolean = o match {
      case e: MutableIntMap[_]#MapEntry =>
        getKey == e.getKey && getValue == e.getValue
      case _ => false
    }

    override def hashCode(): Int = {
      key ^ (if (getValue == null) 0 else getValue.hashCode())
    }

    override def toString: String = s"$key=$getValue"
  }

  override def clear(): Unit = {
    tree = new Array[Int](128)
    _values = new Array[Any](4)
    tree(0) = -1
    tree(1) = -1
    treeSize = 2
    _size = 0
    availableTreeNode.clear()
    availableValuePos.clear()
  }

  override def size: Int = _size

  override def isEmpty: Boolean = _size == 0

  protected def createNode(): Int = {
    if (availableTreeNode.isEmpty) {
      if (treeSize + 2 > tree.length) {
        val newTree = new Array[Int](tree.length * 2)
        System.arraycopy(tree, 0, newTree, 0, treeSize)
        tree = newTree
      }
      tree(treeSize) = -1
      tree(treeSize + 1) = -1
      treeSize += 2
      treeSize - 2
    } else {
      val pos = availableTreeNode.pop()
      tree(pos) = -1
      tree(pos + 1) = -1
      pos
    }
  }

  protected def createValue(): Int = {
    if (availableValuePos.isEmpty) {
      if (_size == _values.length) {
        val newArray = new Array[Any](_values.length * 2)
        System.arraycopy(_values, 0, newArray, 0, _size)
        _values = newArray
      }
      _values(_size) = null
      _size += 1
      _size - 1
    } else {
      val pos = availableValuePos.pop()
      _values(pos) = null
      _size += 1
      pos
    }
  }

  protected def findIdx(key: Int, create: Boolean): Int = {
    pathBuffer(0) = 0
    var i = 0
    while (i < 31) {
      val node = pathBuffer(i)
      val direction = node + ((key >> (31 - i)) & 1)
      val nextNode = tree(direction)

      if (nextNode != -1) {
        pathBuffer(i + 1) = nextNode
      } else if (!create) {
        pathBuffer(i + 1) = -1
        return -i - 1
      } else {
        pathBuffer(i + 1) = createNode()
        tree(direction) = pathBuffer(i + 1)
      }
      i += 1
    }
    val lastNode = pathBuffer(31) + (key & 1)
    if (tree(lastNode) != -1) tree(lastNode)
    else if (!create) -32
    else {
      tree(lastNode) = createValue()
      tree(lastNode)
    }
  }

  override def contains(key: Int): Boolean = {
    val idx = findIdx(convertKey(key), false)
    idx >= 0
  }

  override def get(key: Int): Option[V] = {
    val realKey = convertKey(key)
    val idx = findIdx(realKey, false)
    if (idx >= 0) Some(_values(idx).asInstanceOf[V])
    else None
  }

  override def update(key: Int, value: V): Unit = put(key, value)
  
  override def put(key: Int, value: V): Option[V] = {
    val realKey = convertKey(key)
    val idx = findIdx(realKey, true)
    val old = _values(idx)
    _values(idx) = value
    if (old != null) Some(old.asInstanceOf[V]) else None
  }

  override def subtractOne(key: Int): this.type = {
    remove(key)
    this
  }

  override def addOne(kv: (Int, V)): this.type = {
    put(kv._1, kv._2)
    this
  }

  override def remove(key: Int): Option[V] = {
    val realKey = convertKey(key)
    val idx = findIdx(realKey, false)
    if (idx < 0) return None
    val originalValue = _values(idx)
    availableValuePos.push(idx)
    _size -= 1

    var i = 31
    while (i >= 0) {
      val node = pathBuffer(i)
      if (i != 0 && (tree(node) == -1 || tree(node + 1) == -1)) {
        tree(node) = -1
        tree(node + 1) = -1
        availableTreeNode.push(node)
      } else {
        if (i == 31) {
          if (tree(node) == idx) tree(node) = -1
          else tree(node + 1) = -1
        } else if (tree(pathBuffer(i)) == pathBuffer(i + 1)) {
          tree(pathBuffer(i)) = -1
        } else {
          tree(pathBuffer(i) + 1) = -1
        }
        return Some(originalValue.asInstanceOf[V])
      }
      i -= 1
    }
    throw new RuntimeException("This should never be reached")
  }

  private def findLowestNodeGoingToXAndFollow(key: Int, left: Boolean, strict: Boolean): Option[MapEntry] = {
    var mutKey = key
    val idx = findIdx(mutKey, false)

    if (!strict && idx >= 0) {
      return Some(new MapEntry(convertKey(mutKey), idx))
    }

    val reachedLastLevel = idx >= 0 || idx == -32
    if (reachedLastLevel && left && (mutKey & 1) == 1 && tree(pathBuffer(31)) != -1) {
      mutKey &= ~1
      return Some(new MapEntry(convertKey(mutKey), tree(pathBuffer(31))))
    }
    if (reachedLastLevel && !left && (mutKey & 1) == 0 && tree(pathBuffer(31) + 1) != -1) {
      mutKey |= 1
      return Some(new MapEntry(convertKey(mutKey), tree(pathBuffer(31) + 1)))
    }

    val offset = if (left) 0 else 1
    var level = -1
    val startLevel = if (reachedLastLevel) 30 else -idx - 1
    var i = startLevel
    while (i >= 0 && level == -1) {
      val nextNode = tree(pathBuffer(i) + offset)
      if (nextNode != -1 && nextNode != pathBuffer(i + 1)) {
        level = i
      }
      i -= 1
    }
    if (level == -1) return None

    if (left) {
      pathBuffer(level + 1) = tree(pathBuffer(level))
      mutKey &= ~(1 << (31 - level))
    } else {
      pathBuffer(level + 1) = tree(pathBuffer(level) + 1)
      mutKey |= (1 << (31 - level))
    }
    level += 1

    if (left) {
      for (j <- level until 31) {
        if (tree(pathBuffer(j) + 1) != -1) {
          pathBuffer(j + 1) = tree(pathBuffer(j) + 1)
          mutKey |= (1 << (31 - j))
        } else {
          pathBuffer(j + 1) = tree(pathBuffer(j))
          mutKey &= ~(1 << (31 - j))
        }
      }
      if (tree(pathBuffer(31) + 1) != -1)
        Some(new MapEntry(convertKey(mutKey | 1), tree(pathBuffer(31) + 1)))
      else
        Some(new MapEntry(convertKey(mutKey & ~1), tree(pathBuffer(31))))
    } else {
      for (j <- level until 31) {
        if (tree(pathBuffer(j)) != -1) {
          pathBuffer(j + 1) = tree(pathBuffer(j))
          mutKey &= ~(1 << (31 - j))
        } else {
          pathBuffer(j + 1) = tree(pathBuffer(j) + 1)
          mutKey |= (1 << (31 - j))
        }
      }
      if (tree(pathBuffer(31)) != -1)
        Some(new MapEntry(convertKey(mutKey & ~1), tree(pathBuffer(31))))
      else
        Some(new MapEntry(convertKey(mutKey | 1), tree(pathBuffer(31) + 1)))
    }
  }

  def lowerEntry(key: Int): Option[MapEntry] = {
    if (isEmpty) None
    else findLowestNodeGoingToXAndFollow(convertKey(key), true, true)
  }

  def floorEntry(key: Int): Option[MapEntry] = {
    if (isEmpty) None
    else findLowestNodeGoingToXAndFollow(convertKey(key), true, false)
  }

  def higherEntry(key: Int): Option[MapEntry] = {
    if (isEmpty) None
    else findLowestNodeGoingToXAndFollow(convertKey(key), false, true)
  }

  def ceilingEntry(key: Int): Option[MapEntry] = {
    if (isEmpty) None
    else findLowestNodeGoingToXAndFollow(convertKey(key), false, false)
  }

  def firstEntry(): Option[MapEntry] = ceilingEntry(Int.MinValue)

  def lastEntry(): Option[MapEntry] = floorEntry(Int.MaxValue)

  def pollFirstEntry(): Option[MapEntry] = {
    firstEntry().map { entry =>
      entry.unbind()
      remove(entry.getKey)
      entry
    }
  }

  def pollLastEntry(): Option[MapEntry] = {
    lastEntry().map { entry =>
      entry.unbind()
      remove(entry.getKey)
      entry
    }
  }

  def lowerKey(key: Int): Option[Int] = lowerEntry(key).map(_.getKey)
  def floorKey(key: Int): Option[Int] = floorEntry(key).map(_.getKey)
  def ceilingKey(key: Int): Option[Int] = ceilingEntry(key).map(_.getKey)
  def higherKey(key: Int): Option[Int] = higherEntry(key).map(_.getKey)
  def firstKey(): Option[Int] = firstEntry().map(_.getKey)
  def lastKey(): Option[Int] = lastEntry().map(_.getKey)

  override def iterator: Iterator[(Int, V)] = new Iterator[(Int, V)] {
    private var current: Option[MapEntry] = None
    private var nextEntry: Option[MapEntry] = firstEntry()

    override def hasNext: Boolean = nextEntry.isDefined

    override def next(): (Int, V) = {
      if (nextEntry.isEmpty) throw new NoSuchElementException()
      current = nextEntry
      nextEntry = higherEntry(current.get.getKey)
      (current.get.getKey, current.get.getValue)
    }
  }
}
