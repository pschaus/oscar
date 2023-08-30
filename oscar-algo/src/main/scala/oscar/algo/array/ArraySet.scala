package oscar.algo.array

final class ArraySet(maxValue: Int, initEmpty: Boolean) {

  private[this] val maxSize = maxValue + 1
  private[this] val _values: Array[Int] = Array.tabulate(maxSize)(i => i)
  private[this] val _positions: Array[Int] = Array.tabulate(maxSize)(i => i)
  private[this] var _size: Int = if (initEmpty) 0 else maxSize

  @inline def size: Int = _size

  @inline def isEmpty: Boolean = _size == 0

  @inline def values: Array[Int] = _values

  def add(value: Int): Unit = {
    assert(value <= maxValue && value >= 0) // check value
    val p1 = _positions(value)
    if (p1 >= _size) {
      val v2 = _values(_size)
      _positions(value) = _size
      _positions(v2) = p1
      _values(p1) = v2
      _values(_size) = value
      _size += 1
    }
  }

  def remove(value: Int): Unit = {
    assert(value <= maxValue && value >= 0) // check value
    val p1 = _positions(value)
    if (p1 < _size) {
      _size -= 1
      val v2 = _values(_size)
      _positions(value) = _size
      _positions(v2) = p1
      _values(p1) = v2
      _values(_size) = value
    }
  }

  def addExcluded(value: Int): Unit = {
    assert(value <= maxValue && value >= 0) // check value
    assert(_positions(value) >= _size)
    val p1 = _positions(value)
    val v2 = _values(_size)
    _positions(value) = _size
    _positions(v2) = p1
    _values(p1) = v2
    _values(_size) = value
    _size += 1
  }

  def removeIncluded(value: Int): Unit = {
    assert(value <= maxValue && value >= 0) // check value
    assert(_positions(value) < _size)
    val p1 = _positions(value)
    _size -= 1
    val v2 = _values(_size)
    _positions(value) = _size
    _positions(v2) = p1
    _values(p1) = v2
    _values(_size) = value
  }

  def contains(value: Int): Boolean = {
    assert(value <= maxValue && value >= 0) // check value
    _positions(value) < _size
  }

  def copyToArray(array: Array[Int]): Unit = {
    System.arraycopy(_values, 0, array, 0, _size)
  }

}