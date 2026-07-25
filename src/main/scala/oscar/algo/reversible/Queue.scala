package oscar.algo.reversible

class Queue[T](protected var next: Queue[T], protected var elem: T) {

  def hasNext: Boolean = this.next != null

  def getNext: Queue[T] = next

  def getElem: T = elem

  override def toString: String = {
    var res = ""
    var q = this
    while (q != null) {
      val e = q.getElem
      res += e.toString + (if (q.hasNext) "->" else "")
      q = q.getNext
    }
    res
  }

  def getSize: Int = {
    var size = 0
    var q = this
    while (q != null) {
      size += 1
      q = q.getNext
    }
    size
  }
}
