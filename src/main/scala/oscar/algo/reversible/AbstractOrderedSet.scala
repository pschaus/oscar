package oscar.algo.reversible

abstract class AbstractOrderedSet(val min: Int, val max: Int) extends Iterable[Int] {
  require(max >= min)

  protected def setSize(size: Int): Unit
  def getSize(): Int
  
  protected def setFirst(f: Int): Unit
  def getFirst(): Int
  
  protected def setLast(l: Int): Unit
  def getLast(): Int
  
  protected def setNext(i: Int, v: Int): Unit
  def getNext(i: Int): Int
  
  protected def setPrev(i: Int, v: Int): Unit
  def getPrev(i: Int): Int

  protected def initSet(): Unit = {
    setSize(max - min + 1)
    setFirst(0)
    setLast(getSize() - 1)
    var i = 0
    while (i < getSize()) {
      setNext(i, i + 1)
      setPrev(i, i - 1)
      i += 1
    }
    setNext(getSize() - 1, -1)
  }

  def hasValue(v: Int): Boolean = {
    getSize() > 0 && v >= min && v <= max && (getFirst() == v - min || (getNext(v - min) >= 0) || (getPrev(v - min) >= 0))
  }

  private def isFirst(v: Int): Boolean = {
    (v - min) == getFirst()
  }

  private def isLast(v: Int): Boolean = {
    (v - min) == getLast()
  }

  def removeValue(v: Int): Unit = {
    if (hasValue(v)) {
      if (getSize() == 1) {
        setFirst(getSize()) // this mimics getSize in Java code which was max-min+1 before setting it
        setLast(-1)
      } else if (isFirst(v)) {
        assert(getSize() >= 2)
        setPrev(getNext(getFirst()), -1)
        setFirst(getNext(getFirst()))
      } else if (isLast(v)) {
        assert(getSize() >= 2)
        setNext(getPrev(getLast()), -1)
        setLast(getPrev(getLast()))
      } else {
        assert(getSize() > 2)
        val next_old = getNext(v - min)
        val prev_old = getPrev(v - min)
        setNext(prev_old, next_old)
        setPrev(next_old, prev_old)
      }
      setPrev(v - min, -1)
      setNext(v - min, -1)
      setSize(getSize() - 1)
    }
  }

  def hasNext(i: Int): Boolean = {
    getNext(i) != -1
  }

  def hasPrev(i: Int): Boolean = {
    getPrev(i) != -1
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    private var iterIndex = getFirst()

    override def hasNext: Boolean = getSize() > 0 && iterIndex != -1

    override def next(): Int = {
      if (!hasNext) throw new NoSuchElementException()
      val res = iterIndex + AbstractOrderedSet.this.min
      iterIndex = getNext(iterIndex)
      res
    }
  }
}
