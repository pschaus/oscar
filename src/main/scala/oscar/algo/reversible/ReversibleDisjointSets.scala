package oscar.algo.reversible


/**
 * ReversibleDisjointSets
 * 
 * Maintains a reversible set S = {S_1, S_2, ..., S_k} of disjoint sets S_i. 
 * Initially, each element in 0 until n is contained in a singleton set. 
 *
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleDisjointSets(node: ReversibleContext, private val n: Int) {

  // Initially, each element is the root of its tree and has a rank of 0.
  private val father: Array[ReversibleInt] = Array.tabulate(n)(new ReversibleInt(node, _))
  private val rank: Array[ReversibleInt] = Array.fill(n)(new ReversibleInt(node, 0))

  /** Joins the two sets containing i and j (i and j have to be in different sets). */
  def union(i: Int, j: Int): Unit = {
    link(findSet(i), findSet(j))
  }

  /** Returns the identifier of the set containing i. */
  def findSet(i: Int): Int = {
    // Path compression if i is not the root
    if (i != father(i).value) {
      father(i).value = findSet(father(i).value)
    }
    father(i).value
  }

  /** Returns true if i and j are contained in the same set, false otherwise. */
  def sameSet(i: Int, j: Int): Boolean = {
    findSet(i) == findSet(j)
  }

  // Help to reduce the size of the trees
  private def link(i: Int, j: Int): Unit = {
    if (rank(i).value > rank(j).value) {
      father(j).value = i
    } else {
      father(i).value = j
      if (rank(i).value == rank(j).value) {
        rank(j).incr()
      }
    }
  }
}

object ReversibleDisjointSets {
  def apply(node: ReversibleContext, n: Int): ReversibleDisjointSets = {
    new ReversibleDisjointSets(node, n)
  }
}
