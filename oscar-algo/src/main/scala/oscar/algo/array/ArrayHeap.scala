package oscar.algo.array

class ArrayHeap(initSize: Int, val keys: Array[Int]) {

  private var heap: Array[Int] = new Array[Int](initSize)

  private var heapSize: Int = 0

  final def head: Int = heap(1)

  final def size: Int = heapSize

  final def isEmpty: Boolean = heapSize == 0

  final def removeAll(): Unit = heapSize = 0

  @inline final def enqueue(elem: Int): Unit = {   
    // Position of the new element
    heapSize += 1 
    // Extends the size of the heap
    if (heapSize == heap.length) {
      val newHeap = new Array[Int](heapSize << 1)
      System.arraycopy(heap, 0, newHeap, 0, heapSize)
      heap = newHeap
    }
    // Enqueue the new element
    heap(heapSize) = elem
    heapifyBottomUp(heapSize)
  }

  @inline final def dequeue(): Int = {
    if (isEmpty) throw new NoSuchElementException("empty")
    else {
      val min = heap(1)
      heap(1) = heap(heapSize)
      heapSize -= 1
      heapifyTopDown(1)
      min
    }
  }

  @annotation.tailrec
  @inline private final def heapifyTopDown(i: Int): Unit = {
    val min = minSon(i)
    if (min != i) {
      val temp = heap(i)
      heap(i) = heap(min)
      heap(min) = temp
      heapifyTopDown(min)
    }
  }
  
  @annotation.tailrec
  @inline private final def heapifyBottomUp(i: Int): Unit = { 
    if (i > 1) {
      val p = i >> 1 // parent
      if (keys(heap(p)) > keys(heap(i))) {
        val temp = heap(i)
        heap(i) = heap(p)
        heap(p) = temp
        heapifyBottomUp(p)
      }
    }
  }

  @inline private final def minSon(i: Int): Int = {
    val l = i << 1 // left
    val r = l + 1 // right
    var min = i
    if (l <= heapSize) {
      if (keys(heap(l)) < keys(heap(i))) min = l
      else min = i
      if (r <= heapSize && keys(heap(r)) < keys(heap(min))) {
        min = r
      }
    }
    min
  }
  
  override def toString: String = "ArrayHeap("+heap.take(heapSize+1).mkString(", ")+")"
}