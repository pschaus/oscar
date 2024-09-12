package oscar.cp.scheduling.constraints

import oscar.cp.core.variables.CPIntVar
import scala.annotation.tailrec
import scala.math.{min, max}

/**
 * @author Steven Gay steven.gay@uclouvain.be
 */

/* 
 *  A cumulative theta tree. Envelope/workload must be Long, to be able to handle large tasks and horizons.
 *  Workflow:
 *  _ fill by events
 *  _ call reset(nEvents), where nEvents is the number of events.
 *  _ insert/move/remove events in theta and lambda
 *  _ ask for envelope, enevelopeOpt, event causing envelopeOpt...
 */

class CumulativeLambdaThetaTree(eventEnvelope: Array[Long], eventWorkload: Array[Long]) {
  private def nextPowerOfTwo(k: Int): Int = {
    if (k < 2) 
      2  // we must be able to read envelope(1) 
    else
      1 << math.ceil(math.log(k) / math.log(2)).toInt
  }
  
  private val maxNodes = 2 * nextPowerOfTwo(eventEnvelope.length)
  
  private[this] var C = 0L
  private[this] var nNodes = 0
  
  private[this] val envelope    = Array.ofDim[Long](maxNodes)
  private[this] val workload    = Array.ofDim[Long](maxNodes)
  private[this] val envelopeOpt = Array.ofDim[Long](maxNodes)
  private[this] val workloadOpt = Array.ofDim[Long](maxNodes)
  
  def thetaEnvelope  = envelope(1)
  def lambdaEnvelope = envelopeOpt(1)
  
  def isEventInTheta(event: Int)  = workload(event + nNodes) > 0
  def isEventInLambda(event: Int) = workloadOpt(event + nNodes) > 0

  def reset(nEvents: Int) = {
    nNodes = nextPowerOfTwo(nEvents)
    
    // fill first nodes with empty values
    var p = 2 * nNodes
    while (p > 0) {
      p -= 1
      workload(p) = 0L
      envelope(p) = Long.MinValue
      workloadOpt(p) = 0L
      envelopeOpt(p) = Long.MinValue
    }
  }

  
  @tailrec private def update(node: Int): Unit = {
    if (node > 0) {
      val left = node << 1
      val right = 1 + left
      
      workload(node) = workload(left) + workload(right)
      envelope(node) = max(envelope(left) + workload(right), envelope(right))
      
      workloadOpt(node) = max(workloadOpt(left) + workload(right),
                              workload(left) + workloadOpt(right))
                           
      envelopeOpt(node) = max(envelopeOpt(right),
                          max(envelope(left) + workloadOpt(right),
                              envelopeOpt(left) + workload(right)
                          ))
      update(node >> 1)
    }
  }

  def addToTheta(event: Int) = {
    val node = event + nNodes
    workload(node)    = eventWorkload(event)
    envelope(node)    = eventWorkload(event) + eventEnvelope(event) 
    workloadOpt(node) = eventWorkload(event)
    envelopeOpt(node) = eventWorkload(event) + eventEnvelope(event) 
    update(node >> 1)
  }
  
  // can be used to move from theta to lambda
  def addToLambda(event: Int) = {
    val node = event + nNodes
    workload(node)    = 0
    envelope(node)    = Long.MinValue 
    workloadOpt(node) = eventWorkload(event)
    envelopeOpt(node) = eventWorkload(event) + eventEnvelope(event) 
    update(node >> 1)
  }
  
  def remove(event: Int) = {
    val node = event + nNodes
    workload(node)    = 0
    envelope(node)    = Long.MinValue 
    workloadOpt(node) = 0
    envelopeOpt(node) = Long.MinValue 
    update(node >> 1)
  }
  
  def getLambdaEvent(): Int = getLambdaEvent(1)
  
  // Find which optional event causes the value of energyOpt
  @tailrec private def getLambdaEvent(node: Int): Int = {
    if (node >= nNodes) node - nNodes  // reached a leaf
    else {
      val left = node << 1
      val right = 1 + left
      val e = envelopeOpt(node) 
      
      if (e == envelopeOpt(right)) {
        getLambdaEvent(right)
      }
      else if (e == envelopeOpt(left) + workload(right)) {
        getLambdaEvent(left)
      }
      else {
        getLambdaEventWorkload(right)
      }
    }
  }
  
  // Find which optional event causes the value of workloadOpt
  @tailrec private def getLambdaEventWorkload(node: Int): Int = {
    if (node >= nNodes) node - nNodes  // reached a leaf
    else {
      val left = node << 1
      val right = 1 + left
      val w = workloadOpt(node)
      
      if (w == workloadOpt(left) + workload(right)) {
        getLambdaEventWorkload(left)
      }
      else {
        getLambdaEventWorkload(right)
      }
    }
  }
  
}
