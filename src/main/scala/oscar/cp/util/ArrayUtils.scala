package oscar.cp.util

import oscar.cp.core.variables.CPIntVar
import scala.util.Random

object ArrayUtils {

  @scala.annotation.varargs
  def max(a: Int*): Int = {
    var v = Int.MinValue
    for (i <- a) {
      v = Math.max(v, i)
    }
    v
  }

  def max(a: Array[Array[Int]]): Int = {
    var v = Int.MinValue
    for (i <- a) {
      v = Math.max(v, max(i: _*))
    }
    v
  }

  @scala.annotation.varargs
  def min(a: Int*): Int = {
    var v = Int.MaxValue
    for (i <- a) {
      v = Math.min(v, i)
    }
    v
  }

  def min(a: Array[Array[Int]]): Int = {
    var v = Int.MaxValue
    for (i <- a) {
      v = Math.min(v, min(i: _*))
    }
    v
  }

  def argMax(a: Array[Int]): Int = {
    var v = Int.MinValue
    var ind = -1
    for (i <- a.indices) {
      if (a(i) > v) {
        v = a(i)
        ind = i
      }
    }
    ind
  }

  def argMin(a: Array[Int]): Int = {
    var v = Int.MaxValue
    var ind = -1
    for (i <- a.indices) {
      if (a(i) < v) {
        v = a(i)
        ind = i
      }
    }
    ind
  }

  def sum(a: Array[Int]): Int = {
    var v = 0
    for (i <- a) {
      v += i
    }
    v
  }

  def sum(a: Array[Double]): Double = {
    var v = 0.0
    for (i <- a) {
      v += i
    }
    v
  }

  def sum(a: Array[Array[Int]]): Int = {
    var s = 0
    for (i <- a) {
      s += sum(i)
    }
    s
  }

  def prod(a: Array[Int]): Int = {
    var v = 1
    for (i <- a) {
      v *= i
    }
    v
  }

  def append(a1: Array[CPIntVar], a2: Array[CPIntVar]): Array[CPIntVar] = {
    a1 ++ a2
  }

  def getSlice(a: Array[Array[CPIntVar]], c: Int): Array[CPIntVar] = {
    val res = new Array[CPIntVar](a.length)
    for (i <- a.indices) {
      res(i) = a(i)(c)
    }
    res
  }

  def getSlice(a: Array[Array[Int]], c: Int): Array[Int] = {
    val res = new Array[Int](a.length)
    for (i <- a.indices) {
      res(i) = a(i)(c)
    }
    res
  }

  def replicate(v: Int, n: Int): Array[Int] = {
    Array.fill(n)(v)
  }

  def flatten[E](a: Array[Array[E]]): java.util.ArrayList[E] = {
    val res = new java.util.ArrayList[E]()
    for (i <- a) {
      for (j <- i) {
        res.add(j)
      }
    }
    res
  }

  def flattenvars(a: Array[Array[CPIntVar]]): Array[CPIntVar] = {
    flatten(a).toArray(new Array[CPIntVar](0))
  }

  def getMinVal(a: Array[CPIntVar]): Int = {
    var res = a(0).getMin
    for (x <- a) {
      if (x.getMin < res) {
        res = x.getMin
      }
    }
    res
  }

  def getMaxVal(a: Array[CPIntVar]): Int = {
    var res = a(0).getMax
    for (x <- a) {
      if (x.getMax > res) {
        res = x.getMax
      }
    }
    res
  }

  def getFirstNotBound(x: Array[CPIntVar]): Int = {
    for (i <- x.indices) {
      if (!x(i).isBound) return i
    }
    -1
  }

  def getMinValNotBound(x: Array[CPIntVar]): Int = {
    var value = Int.MaxValue
    var varIdx = -1
    for (i <- x.indices) {
      if (!x(i).isBound && x(i).getMin < value) {
        varIdx = i
        value = x(i).getMin
      }
    }
    varIdx
  }

  def getRandomNotBound(x: Array[CPIntVar]): Int = {
    val rand = x(0).store.getRandom
    var cpt = 0
    var curr = -1
    for (i <- x.indices) {
      if (!x(i).isBound) {
        cpt += 1
        if (rand.nextInt(cpt) == 0) {
          curr = i
        }
      }
    }
    curr
  }

  def getMinDomNotBound(x: Array[CPIntVar]): Int = {
    getVarNotBound(x, new java.util.Comparator[Integer] {
      def compare(o1: Integer, o2: Integer): Int = x(o1).getSize - x(o2).getSize
    })
  }

  @scala.annotation.varargs
  def getVarNotBound(x: Array[CPIntVar], comp: java.util.Comparator[Integer]*): Int = {
    var ind = -1
    var found = false
    var i = 0
    while(i < x.length && !found) {
      if (!x(i).isBound) {
        ind = i
        found = true
      }
      i += 1
    }
    if (ind == -1) return -1
    
    i = ind + 1
    while(i < x.length) {
      if (!x(i).isBound) {
        var cIdx = 0
        var done = false
        while (cIdx < comp.length && !done) {
          val c = comp(cIdx)
          if (c.compare(i, ind) > 0) {
            done = true
          } else if (c.compare(i, ind) < 0) {
            ind = i
            done = true
          }
          cIdx += 1
        }
      }
      i += 1
    }
    ind
  }

  def getMaxBoundVal(x: Array[CPIntVar]): Int = {
    var v = Int.MinValue
    for (i <- x.indices) {
      if (x(i).isBound) {
        v = Math.max(v, x(i).getMin)
      }
    }
    v
  }

  def getRandomPermutation(n: Int, seed: Int): Array[Int] = {
    val perm = Array.tabulate(n)(identity)
    val rand = new java.util.Random(seed)
    for (i <- perm.indices) {
      val ind1 = rand.nextInt(n)
      val ind2 = rand.nextInt(n)
      val temp = perm(ind1)
      perm(ind1) = perm(ind2)
      perm(ind2) = temp
    }
    perm
  }

  def applyPermutation[E](x: Array[E], permutation: Array[Int]): Unit = {
    val objs = x.clone()
    for (i <- permutation.indices) {
      x(i) = objs(permutation(i))
    }
  }

  def applyPermutation(x: Array[Int], permutation: Array[Int]): Unit = {
    val xcopy = x.clone()
    for (i <- permutation.indices) {
      x(i) = xcopy(permutation(i))
    }
  }

  def sortPerm(w: Array[Int]): Array[Int] = {
    val perm = Array.tabulate(w.length)(i => Integer.valueOf(i))
    java.util.Arrays.sort(perm, new java.util.Comparator[Integer] {
      def compare(o1: Integer, o2: Integer): Int = w(o1) - w(o2)
    })
    perm.map(_.intValue)
  }

  def sort[E](x: Array[E], w: Array[Int]): Unit = {
    applyPermutation(x, sortPerm(w))
  }
}
