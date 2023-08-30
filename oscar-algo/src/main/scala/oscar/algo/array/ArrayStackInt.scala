package oscar.algo.array

/**
 *  An array-based stack for integer.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class ArrayStackInt(initialSize: Int = 100) {

  private[this] var stack: Array[Int] = Array.ofDim[Int](initialSize)
  private[this] var index: Int = 0

  /**
   *  Return the size of the stack
   *
   *  @return The size of the stack
   */
  @inline final def size: Int = index
  
  /**
   *  Return the size of the stack
   *
   *  @return The size of the stack
   */
  @inline final def length: Int = index

  /**
   *  Test if the stack is empty or not
   *
   *  @return `true` if the stack is empty, `false` otherwise
   */
  @inline final def isEmpty = index == 0

  /**
   *  Return the top element of the stack without removing it
   *
   *  Throws an exception if the stack is empty
   *
   *  @return The top element of the stack
   */
  @inline final def top: Int = stack(index - 1)

  /**
   *  Return the last element of the stack in LIFO order
   *
   *  Throws an exception if the stack is empty
   *
   *  @return The last element of the stack in LIFO order
   */
  @inline final def last: Int = {
    if (index == 0) sys.error("Stack empty")
    else stack(0)
  }

  /**
   *  Push an element onto the stack.
   *
   *  @param entry The element to push
   */
  @inline final def push(entry: Int): Unit = {
    if (index == stack.length) growStack()
    stack(index) = entry
    index += 1
  }
  
  /**
   *  Push an element onto the stack.
   *
   *  @param entry The element to push
   */
  @inline final def append(entry: Int): Unit = push(entry)

  /**
   *  Pop the element on top of the stack
   *
   *  @return The element on top of the stack
   */
  @inline final def pop(): Int = {
    if (index == 0) sys.error("Stack empty")
    index -= 1
    stack(index)
  }

  /**
   *  Remove all the entries in the stack without removing references
   *  in the internal structure. This means that object cannot be
   *  garbage collected until references are overriden.
   */
  @inline final def clear(): Unit = index = 0
  
  @inline final def foreach[U](f: Int => U): Unit = {
    var i = index
    while(i > 0) {
      i -= 1
      f(stack(i))
    }
  }
  
  @inline def apply(idx: Int): Int = {
    if (idx >= index) throw new IndexOutOfBoundsException
    else stack(idx)
  }

  @inline def update(idx: Int, entry: Int): Unit = {
    if (idx >= index) throw new IndexOutOfBoundsException
    else stack(idx) = entry
  }
  
  // Double the size of the stack
  @inline private def growStack(): Unit = {
    val newStack = new Array[Int](stack.length * 2)
    System.arraycopy(stack, 0, newStack, 0, stack.length)
    stack = newStack
  }
  
  @inline final def toArray: Array[Int] = {
    val array = new Array[Int](index)
    System.arraycopy(stack, 0, array, 0, index)
    array
  }
}
