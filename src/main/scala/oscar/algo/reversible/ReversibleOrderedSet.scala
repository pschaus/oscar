package oscar.algo.reversible

class ReversibleOrderedSet(n: ReversibleContext, min: Int, max: Int) extends AbstractOrderedSet(min, max) {
  private val size: ReversibleInt = new ReversibleInt(n, 0)
  private val first: ReversibleInt = new ReversibleInt(n, 0)
  private val last: ReversibleInt = new ReversibleInt(n, 0)
  private val prev: Array[ReversibleInt] = new Array[ReversibleInt](max - min + 1)
  private val next: Array[ReversibleInt] = new Array[ReversibleInt](max - min + 1)

  for (i <- 0 until (max - min + 1)) {
    prev(i) = new ReversibleInt(n, 0)
    next(i) = new ReversibleInt(n, 0)
  }
  
  initSet()

  override protected def setSize(size: Int): Unit = {
    this.size.setValue(size)
  }

  override def getSize(): Int = this.size.getValue()

  override protected def setFirst(f: Int): Unit = {
    this.first.setValue(f)
  }

  override def getFirst(): Int = first.getValue()

  override protected def setLast(l: Int): Unit = {
    this.last.setValue(l)
  }

  override def getLast(): Int = last.getValue()

  override protected def setNext(i: Int, v: Int): Unit = {
    this.next(i).setValue(v)
  }

  override def getNext(i: Int): Int = next(i).getValue()

  override protected def setPrev(i: Int, v: Int): Unit = {
    prev(i).setValue(v)
  }

  override def getPrev(i: Int): Int = prev(i).getValue()
}
