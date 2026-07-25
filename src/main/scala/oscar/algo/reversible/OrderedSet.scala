package oscar.algo.reversible

class OrderedSet(min: Int, max: Int) extends AbstractOrderedSet(min, max) {
  private var _size: Int = 0
  private var _first: Int = 0
  private var _last: Int = 0
  private val prev: Array[Int] = new Array[Int](max - min + 1)
  private val next: Array[Int] = new Array[Int](max - min + 1)

  initSet()

  override protected def setSize(size: Int): Unit = {
    this._size = size
  }

  override def getSize(): Int = _size

  override protected def setFirst(f: Int): Unit = {
    this._first = f
  }

  override def getFirst(): Int = _first

  override protected def setLast(l: Int): Unit = {
    this._last = l
  }

  override def getLast(): Int = _last

  override protected def setNext(i: Int, v: Int): Unit = {
    this.next(i) = v
  }

  override def getNext(i: Int): Int = next(i)

  override protected def setPrev(i: Int, v: Int): Unit = {
    prev(i) = v
  }

  override def getPrev(i: Int): Int = prev(i)
}
