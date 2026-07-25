package oscar.algo.reversible

import scala.collection.mutable

class ReversibleBoundedSet(node: ReversibleContext, maxval: Int) extends Iterable[Int] {
  if (maxval < 0) {
    throw new RuntimeException("maxval must be >= 0")
  }

  private val n: Int = maxval + 1
  private val size: ReversibleInt = new ReversibleInt(node, 0)
  private val _first: ReversibleInt = new ReversibleInt(node, n)

  private val next: Array[ReversibleInt] = new Array[ReversibleInt](n)
  private val prev: Array[ReversibleInt] = new Array[ReversibleInt](n)

  for (i <- 0 until n) {
    next(i) = new ReversibleInt(node, n)
    prev(i) = new ReversibleInt(node, n)
  }

  def insert(valArg: Int): Unit = {
    if (valArg < 0 || valArg >= n) {
      throw new RuntimeException(s"val must be between 0 and ${n - 1}")
    }
    val i = _first.getValue()
    if (i == n) {
      _first.setValue(valArg)
      size.incr()
    } else if (!contains(valArg)) {
      next(valArg).setValue(_first.getValue())
      prev(_first.getValue()).setValue(valArg)
      _first.setValue(valArg)
      size.incr()
    }
  }

  def remove(valArg: Int): Unit = {
    val fv = _first.getValue()
    if (fv != n) {
      if (fv == valArg) {
        if (next(valArg).getValue() == n) {
          _first.setValue(n)
        } else {
          _first.setValue(next(valArg).getValue())
          prev(next(valArg).getValue()).setValue(valArg)
        }
        size.decr()
      } else if (next(valArg).getValue() != n) {
        next(prev(valArg).getValue()).setValue(next(valArg).getValue())
        prev(next(valArg).getValue()).setValue(prev(valArg).getValue())
        next(valArg).setValue(n)
        prev(valArg).setValue(n)
        size.decr()
      } else if (prev(valArg).getValue() != n) {
        next(prev(valArg).getValue()).setValue(n)
        prev(valArg).setValue(n)
        size.decr()
      }
    }
    assert(size.getValue() >= 0)
  }

  def getSize(): Int = size.getValue()

  def first(): Int = _first.getValue()

  def getNext(valArg: Int): Int = next(valArg).getValue()

  def contains(valArg: Int): Boolean = {
    if (_first.getValue() != n) {
      if (_first.getValue() == valArg) {
        return true
      } else if (prev(valArg).getValue() != n) {
        return true
      }
    }
    false
  }

  def getValues: Set[Int] = {
    val res = mutable.TreeSet[Int]()
    var curr = _first.getValue()
    while (curr != n) {
      res.add(curr)
      curr = next(curr).getValue()
    }
    res.toSet
  }

  override def isEmpty: Boolean = size.getValue() == 0

  override def iterator: Iterator[Int] = new Iterator[Int] {
    private var iteVal = first()

    override def hasNext: Boolean = iteVal != n

    override def next(): Int = {
      val oldVal = iteVal
      iteVal = getNext(iteVal)
      oldVal
    }
  }
}
