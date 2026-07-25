package oscar.algo.reversible

import scala.collection.mutable.ArrayBuffer
import java.util.Arrays

abstract class AbstractSparseSet extends Iterable[Int] {
  private var _min: Int = 0
  private var values: Array[Int] = _
  private var indexes: Array[Int] = _

  protected def createSizeMinMax(): Unit
  protected def setSize(size: Int): Unit
  protected def setMin(min: Int): Unit
  protected def setMax(max: Int): Unit
  def getSize(): Int
  def getMin(): Int
  def getMax(): Int

  private def incrSize(): Unit = {
    setSize(getSize() + 1)
  }

  private def decrSize(): Unit = {
    setSize(getSize() - 1)
  }

  protected def initIndexes(min: Int, max: Int, empty: Boolean): Unit = {
    assert(max >= min)
    createSizeMinMax()
    this._min = min
    values = new Array[Int](max - min + 1)
    indexes = new Array[Int](max - min + 1)
    for (i <- values.indices) {
      values(i) = i
      indexes(i) = i
    }
    if (!empty) setSize(values.length)
    else setSize(0)
    setMax(max)
    setMin(min)
  }

  def insert(valArg: Int): Unit = {
    assert(checkVal(valArg))
    if (hasValue(valArg)) return
    else if (isEmpty) {
      setMin(valArg)
      setMax(valArg)
    } else {
      if (valArg > getMax()) setMax(valArg)
      if (valArg < getMin()) setMin(valArg)
    }
    val s = getSize()
    exchangePositions(valArg, values(s) + _min)
    incrSize()
    assert(getSize() <= values.length)
  }

  private def exchangePositions(val1: Int, val2: Int): Unit = {
    assert(checkVal(val1))
    assert(checkVal(val2))
    val v1 = val1 - _min
    val v2 = val2 - _min
    val i1 = indexes(v1)
    val i2 = indexes(v2)
    values(i1) = v2
    values(i2) = v1
    indexes(v1) = i2
    indexes(v2) = i1
  }

  private def checkVal(valArg: Int): Boolean = {
    assert(valArg >= _min)
    assert(valArg <= _min + values.length - 1)
    true
  }

  def toArray: Array[Int] = {
    val res = new Array[Int](getSize())
    fillArray(res)
    res
  }

  def fillArray(dest: Array[Int]): Int = {
    val size = getSize()
    System.arraycopy(values, 0, dest, 0, size)
    if (_min != 0) {
      var i = size
      while (i > 0) {
        i -= 1
        dest(i) += _min
      }
    }
    size
  }

  def clear(): Unit = {
    setSize(0)
  }

  override def isEmpty: Boolean = getSize() == 0

  private def updateBoundsValRemoved(valArg: Int): Unit = {
    updateMaxValRemoved(valArg)
    updateMinValRemoved(valArg)
  }

  private def updateMaxValRemoved(valArg: Int): Unit = {
    if (!isEmpty && getMax() == valArg) {
      assert(!hasValue(valArg))
      var v = valArg - 1
      while (v >= getMin()) {
        if (hasValue(v)) {
          setMax(v)
          return
        }
        v -= 1
      }
    }
  }

  private def updateMinValRemoved(valArg: Int): Unit = {
    if (!isEmpty && getMin() == valArg) {
      assert(!hasValue(valArg))
      var v = valArg + 1
      while (v <= getMax()) {
        if (hasValue(v)) {
          setMin(v)
          return
        }
        v += 1
      }
    }
  }

  def removeValue(valArg: Int): Boolean = {
    assert(checkVal(valArg))
    if (!hasValue(valArg)) return false
    val s = getSize()
    exchangePositions(valArg, values(s - 1) + _min)
    decrSize()
    updateBoundsValRemoved(valArg)
    true
  }

  def hasValue(valArg: Int): Boolean = {
    if (valArg < _min || valArg >= _min + indexes.length) return false
    indexes(valArg - _min) < getSize()
  }

  def getNextValue(valArg: Int): Int = {
    assert(checkVal(valArg))
    assert(!isEmpty)
    var v = valArg
    while (v <= getMax()) {
      if (hasValue(v)) {
        return v
      }
      v += 1
    }
    valArg - 1
  }

  def getPreValue(valArg: Int): Int = {
    assert(checkVal(valArg))
    assert(!isEmpty)
    var v = valArg
    while (v >= getMin()) {
      if (hasValue(v)) {
        return v
      }
      v -= 1
    }
    valArg + 1
  }

  def removeAllBut(v: Int): Unit = {
    assert(checkVal(v))
    assert(hasValue(v))
    val val1 = values(0)
    val index = indexes(v - _min)
    indexes(v - _min) = 0
    values(0) = v - _min
    indexes(val1) = index
    values(index) = val1
    setMin(v)
    setMax(v)
    setSize(1)
  }

  def setMinVal(minArg: Int): Int = {
    assert(checkVal(minArg))
    assert(!isEmpty)
    if (minArg < getMin()) {
      getMin()
    } else if (minArg > getMax()) {
      setSize(0)
      Int.MaxValue
    } else if (minArg == getMax()) {
      removeAllBut(minArg)
      getMin()
    } else {
      var v = getMin()
      while (v < minArg) {
        removeValue(v)
        v += 1
      }
      getMin()
    }
  }

  def setMaxVal(maxArg: Int): Int = {
    assert(checkVal(maxArg))
    assert(!isEmpty)
    if (maxArg >= getMax()) {
      getMax()
    } else if (maxArg < getMin()) {
      setSize(0)
      Int.MinValue
    } else if (maxArg == getMin()) {
      removeAllBut(maxArg)
      getMax()
    } else {
      var v = getMax()
      while (v > maxArg) {
        removeValue(v)
        v -= 1
      }
      getMax()
    }
  }

  def getValues: Array[Integer] = {
    if (isEmpty) return Array.empty[Integer]
    val vals = ArrayBuffer[Integer]()
    var v = getMin()
    while (v <= getMax()) {
      if (hasValue(v)) {
        vals += v
      }
      v += 1
    }
    val valuesArr = vals.toArray
    Arrays.sort(valuesArr.asInstanceOf[Array[AnyRef]])
    valuesArr
  }

  override def toString: String = {
    Arrays.toString(getValues.asInstanceOf[Array[AnyRef]])
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    private var iterIndex = 0

    override def hasNext: Boolean = {
      iterIndex < getSize()
    }

    override def next(): Int = {
      assert(hasNext)
      val i = iterIndex
      iterIndex += 1
      values(i) + _min
    }
  }

  def getSortedVals: Array[Int] = {
    val vals = new Array[Int](getSize())
    var i = 0
    val ite = this.iterator
    while (ite.hasNext) {
      vals(i) = ite.next()
      i += 1
    }
    Arrays.sort(vals)
    vals
  }
}
