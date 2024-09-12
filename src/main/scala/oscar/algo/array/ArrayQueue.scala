package oscar.algo.array

/**
 *  An array-based double ended queue for objects.
 *  This means that primitive types are boxed.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class ArrayQueue[T](initialSize: Int = 8) {

  // The length of this array must be a power of 2
  private var queue: Array[AnyRef] = Array.ofDim[AnyRef](computeSize(initialSize))

  // Used for fast position testing
  private var bitMask: Int = queue.length - 1

  private var head: Int = 0
  private var tail: Int = 0

  /**
   *  Return the size of the queue
   *
   *  @return The size of the queue
   */
  @inline final def size: Int = (tail - head) & bitMask

  /**
   *  Test if the queue is empty or not
   *
   *  @return `true` if the queue is empty, `false` otherwise
   */
  @inline final def isEmpty = head == tail

  /**
   *  Return the first element of the queue without removing it
   *
   *  Throws an exception if the queue is empty
   *
   *  @return The last element of the queue
   */
  @inline final def first: T = {
    val elem = queue(head)
    if (head == tail) sys.error("Queue empty")
    else elem.asInstanceOf[T]
  }

  /**
   *  Return the last element of the queue without removing it
   *
   *  Throws an exception if the queue is empty
   *
   *  @return The last element of the queue
   */
  @inline final def last: T = {
    val elem = queue(tail)
    if (head == tail) sys.error("Queue empty")
    else elem.asInstanceOf[T]
  }

  @inline final def clear(): Unit = {
    head = 0
    tail = 0
  }

  @inline final def addFirst(elem: T): Unit = {
    head = (head - 1) & bitMask
    queue(head) = elem.asInstanceOf[AnyRef]
    if (head == tail) growQueue() // Increase the size of the queue
  }

  @inline final def addLast(elem: T): Unit = {
    queue(tail) = elem.asInstanceOf[AnyRef]
    tail = (tail + 1) & bitMask
    if (head == tail) growQueue() // Increase the size of the queue
  }

  @inline final def removeFirst(): T = {
    if (head == tail) sys.error("Queue empty")
    else {
      val elem = queue(head).asInstanceOf[T]
      head = (head + 1) & bitMask
      elem
    }
  }

  @inline final def removeLast(): T = {
    if (head == tail) sys.error("Queue empty")
    else {
      tail = (tail - 1) & bitMask
      queue(tail).asInstanceOf[T]
    }
  }

  final def foreach[U](f: T => U): Unit = {
    var i = head
    while (i != tail) {
      f(queue(i).asInstanceOf[T])
      i = (i + 1) & bitMask
    }
  }

  final def mkString(start: String, sep: String, end: String): String = {
    val builder = new StringBuilder()
    var first = true
    var i = head
    builder.append(start)
    while (i != tail) {
      if (first) {
        builder.append(queue(i))
        first = false
      } else {
        builder.append(sep)
        builder.append(queue(i))
      }
      i = (i + 1) & (queue.length - 1)
    }
    builder.append(end)
    builder.toString
  }

  final def mkString(sep: String): String = mkString("", sep, "")

  // Double the size of the queue
  @inline private def growQueue(): Unit = {
    // This function does not work if this condition does not hold
    assert(head == tail, "should not resize if head != tail")
    val size = queue.length
    val rest = size - head
    val newSize = size << 1
    if (newSize < 0) sys.error("too many elements")
    else {
      val newQueue = Array.ofDim[AnyRef](newSize)
      System.arraycopy(queue, head, newQueue, 0, rest)
      System.arraycopy(queue, 0, newQueue, rest, head)
      queue = newQueue
      bitMask = newQueue.length - 1
      head = 0
      tail = size
    }
  }

  // Returns the lowest power of 2 that is superior to the initial size
  @inline private def computeSize(oldSize: Int): Int = {
    if (oldSize <= 8) 8
    else {
      var size = initialSize
      size |= (size >>> 1)
      size |= (size >>> 2)
      size |= (size >>> 4)
      size |= (size >>> 8)
      size |= (size >>> 16)
      size += 1
      if (size < 0) size >>> 1 // 2^30 elements
      else size
    }
  }
}
