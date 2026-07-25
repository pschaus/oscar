package oscar.algo.reversible

import java.util.Arrays

class ReversibleSparseSetJava(val s: ReversibleContext, min: Int, max: Int, empty: Boolean = false) extends AbstractSparseSet {
  def this(s: ReversibleContext, min: Int, max: Int) = this(s, min, max, false)
  private var _size: ReversibleInt = _
  private var maxV: ReversibleInt = _
  private var minV: ReversibleInt = _

  initIndexes(min, max, empty)

  override protected def createSizeMinMax(): Unit = {
    _size = new ReversibleInt(s, 0)
    minV = new ReversibleInt(s, 0)
    maxV = new ReversibleInt(s, 0)
  }

  override protected def setSize(size: Int): Unit = {
    this._size.setValue(size)
  }

  override protected def setMin(min: Int): Unit = {
    minV.setValue(min)
  }

  override protected def setMax(max: Int): Unit = {
    maxV.setValue(max)
  }

  override def getSize(): Int = _size.getValue()

  override def getMin(): Int = minV.getValue()

  override def getMax(): Int = maxV.getValue()

  override def toString: String = {
    Arrays.toString(getSortedVals)
  }
}
