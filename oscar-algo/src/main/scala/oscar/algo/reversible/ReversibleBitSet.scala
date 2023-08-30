package oscar.algo.reversible

/**
 * A reversible set with an internal bit-set representation.
 * This set can remove efficiently its elements from another bit-set
 * This set can compute efficiently its intersection with another bit-set
 * In comparison to the ReversibleSparseBitSet, the sparse property is
 * in the internal bit-set representation.
 *
 * Implementation following following the use of bitSet in the paper
 * "Optimizing Simple Tabular Reduction with a Bitwise Representation"
 * Ruiwei Wang, Wei Xia, Roland Yap and Zhanshan Li, IJCAI-16
 *
 * @param context
 * @param n initial values must be taken from {0,...,n-1}
 * @param initialValues the initial values contained in the set
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class ReversibleBitSet(val context: ReversibleContext, val n: Int, val initialValues: Iterable[Int]) {

  /**
   * Immutable sparse bit-set that can be used to remove/intersect
   * with the the ReversibleBitSet
   * @param values initial values, they must be in {0,...,n-1}
   */
  class SparseBitSet(values: Iterable[Int]) {

    protected[ReversibleBitSet] var words: Array[Long] = Array.fill(nWords)(0L)

    protected[ReversibleBitSet] val nonZeroIdx:Array[Int] = Array.fill(nWords)(-1)

    protected[ReversibleBitSet] var nNonZero = new ReversibleInt(context,0)

    assert(values.forall(v => v < n && v >= 0))

    {
      values.foreach(v => BitSetOp.setBit(words, v))
      var i = 0
      var _nNonZero = 0
      while (i < nonZeroIdx.length){
        if (words(i) != 0){
          nonZeroIdx(_nNonZero) = i
          _nNonZero += 1
        }
        i += 1
      }
      nNonZero.setValue(_nNonZero)
    }

    /**
     * @return true if the SparseBitSet is empty
     */
    def isZero: Boolean = {
      var i = nNonZero.getValue()
      while (i > 0){
        i -= 1
        if (words(nonZeroIdx(i)) != 0)
          return false
      }
      true
    }

    val mask: Long = ~0L >>> (64 - (n % 64))

    override def toString: String = {
      val size = n min 64
      words.map(e => String.format(s"%${size}s", java.lang.Long.toBinaryString(e)).replace(' ', '0')).mkString(" ")
    }

  }

  /* Compute number of Long in a bitset */
  private[this] val nWords = BitSetOp.bitLength(n)

  private[this] val words: Array[ReversibleLong] = Array.fill(nWords)(new ReversibleLong(context,0L))

  private[this] val tempMask = Array.fill(nWords)(0L)

  private[this] var lastSupport = 0

  assert(initialValues.forall(v => v < n && v >= 0))

  initialValues.foreach(v => BitSetOp.setBit(words, v))


  def isEmpty(): Boolean = {
    if (words(lastSupport).getValue() != 0L) {
      return false
    }

    var offset: Int = nWords
    while (offset > 0) {
      offset -= 1
      if (words(offset).getValue() != 0L) {
        /* We found a support and we store the index of the Long where the support is */
        lastSupport = offset
        return false
      }
    }

    true
  }

  override def toString(): String = {
    val size = n min 64
    def format(l: Long) = String.format(s"%${size}s", java.lang.Long.toBinaryString(l)).replace(' ', '0')

    "words:" + words.map(format(_)).mkString(" , ")
  }


  /**
   * @param set : the set to do intersection with
   * @return true if set has a non empty intersection with the bit-set
   */
  def intersect(set: SparseBitSet): Boolean = {

    var i: Int = set.nNonZero.getValue()
    while (i > 0) {
      i -= 1
      val offset = set.nonZeroIdx(i)
      if ((words(offset).getValue & set.words(offset)) != 0L) {
        /* We found a support and we store the index of the Long where the support is */
        set.nNonZero.setValue(i+1)
        return true
      }
    }

    false
  }

  /**
   * @param set : the set containing the item to remove
   */
  def remove(set: SparseBitSet): Unit = {
    var i: Int = set.nNonZero.getValue()
    while (i > 0) {
      i -= 1
      val offset = set.nonZeroIdx(i)
      val oldValue = words(offset).getValue()
      val newValue = oldValue & ~set.words(offset)
      words(offset).setValue(newValue)
    }
  }

  /**
   * @param set : the set containing the item
   */
  def intersectWith(set: SparseBitSet): Unit = {
    // need to go through all the words to set to 0
    // the one that doesn't appear in the sparseBitSet
    var offset: Int = nWords
    while (offset > 0) {
      offset -= 1
      val oldValue = words(offset).getValue()
      val newValue = oldValue & set.words(offset)
      words(offset).setValue(newValue)
    }
  }



}