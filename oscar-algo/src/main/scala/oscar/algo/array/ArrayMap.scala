package oscar.algo.array

final class ArrayMap(maxKey: Int, initEmpty: Boolean) {

  private[this] val maxSize = maxKey + 1
  private[this] val _keys: Array[Int] = Array.tabulate(maxSize)(i => i)
  private[this] val _values: Array[Int] = Array.tabulate(maxSize)(i => i)
  private[this] val _positions: Array[Int] = Array.tabulate(maxSize)(i => i)
  private[this] var _size: Int = if (initEmpty) 0 else maxSize

  @inline def size: Int = _size

  @inline def isEmpty: Boolean = _size == 0

  @inline def keys: Array[Int] = _keys
  
  @inline def values: Array[Int] = _values

  def add(key: Int, value: Int): Unit = {
    assert(key <= maxKey && key >= 0) // check key
    val p1 = _positions(key)
    if (p1 >= _size) {
      val k2 = _keys(_size)
      // Swap positions
      _positions(key) = _size
      _positions(k2) = p1
      // Swap keys
      _keys(p1) = k2
      _keys(_size) = key
      // Swap values
      _values(p1) = _values(_size)
      _values(_size) = value
      _size += 1
    }
  }

  def remove(key: Int): Unit = {
    assert(key <= maxKey && key >= 0) // check key
    val p1 = _positions(key)
    if (p1 < _size) {
      _size -= 1
      val k2 = _keys(_size)
      // Swap positions
      _positions(key) = _size
      _positions(k2) = p1
      // Swap keys
      _keys(p1) = k2
      _keys(_size) = key
      // Swap values
      _values(p1) = _values(_size)
    }
  }

  def addExcluded(key: Int, value: Int): Unit = {
    assert(key <= maxKey && key >= 0) // check key
    assert(_positions(key) >= _size)
    val p1 = _positions(key)
    val k2 = _keys(_size)
    // Swap positions
    _positions(key) = _size
    _positions(k2) = p1
    // Swap keys
    _keys(p1) = k2
    _keys(_size) = key
    // Swap values
    _values(p1) = _values(_size)
    _values(_size) = value
    _size += 1
  }

  def removeIncluded(key: Int): Unit = {
    assert(key <= maxKey && key >= 0) // check key
    assert(_positions(key) < _size)
    val p1 = _positions(key)
    _size -= 1
    val k2 = _keys(_size)
    // Swap positions
    _positions(key) = _size
    _positions(k2) = p1
    // Swap keys
    _keys(p1) = k2
    _keys(_size) = key
    // Swap values
    _values(p1) = _values(_size)
  }

  def contains(key: Int): Boolean = {
    assert(key <= maxKey && key >= 0) // check key
    _positions(key)  < _size
  }
}