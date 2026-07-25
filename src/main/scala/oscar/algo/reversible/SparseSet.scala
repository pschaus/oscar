package oscar.algo.reversible

class SparseSet(min: Int, max: Int, empty: Boolean = false) extends AbstractSparseSet {
  def this(min: Int, max: Int) = this(min, max, false)
  private var _size: Int = 0
  private var maxV: Int = 0
  private var minV: Int = 0

  initIndexes(min, max, empty)

  override protected def createSizeMinMax(): Unit = {
    _size = 0
    maxV = 0
    minV = 0
  }

  override protected def setSize(size: Int): Unit = {
    this._size = size
  }

  override protected def setMin(min: Int): Unit = {
    this.minV = min
  }

  override protected def setMax(max: Int): Unit = {
    this.maxV = max
  }

  override def getSize(): Int = _size

  override def getMin(): Int = minV

  override def getMax(): Int = maxV
}
