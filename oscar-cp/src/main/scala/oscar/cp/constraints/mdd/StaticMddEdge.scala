package oscar.cp.constraints.mdd


/**
  * This object is used in the static mdd implementation.
  * When the construction of the static mdd is done, the diagram is turned into a reversible MDD
  *
  * In order to put the edges in a tree/hash set, we implemented the compare, hashCode and euals method, that are
  * based on the id of the edge (each edge has a unique id)
  *
  * @author rhenneton romain.henneton@hotmail.fr
  */

class StaticMddEdge(val topNode: StaticMddNode, val bottomNode: StaticMddNode, val value: Int) extends Ordered[StaticMddEdge] {
  private[this] val id: Long = StaticMddEdge.getId
  topNode.addOutEdge(value, this)
  bottomNode.addInEdge(this)

  def unlink(): Unit = {
    topNode.removeOutEdge(value)
    bottomNode.removeInEdge(this)
  }

  override def compare(that: StaticMddEdge): Int = {
    if (this.id < that.getId) -1
    else if (this.id > that.getId) 1
    else 0
  }


  override def hashCode(): Int = id.toInt

  override def equals(o: scala.Any): Boolean = {
    o match {
      case that: StaticMddEdge => that.getId == this.id
      case _ => false
    }
  }

  def getId: Long = this.id
}


object StaticMddEdge {
  private[this] var edgeId: Long = 0

  private def getId: Long = {
    edgeId += 1
    edgeId
  }
}
