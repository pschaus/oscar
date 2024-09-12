package oscar.algo

/**
  * @author Renaud Hartert ren.hartert@gmail.com
  * @author Guillaume Derval guillaume.derval@uclouvain.be
  */
abstract class Inconsistency extends Exception {
  
  def feedback: Any
  
  // Do not fill the trace
  final override val fillInStackTrace: Throwable = this
  
  final override val toString: String = "Inconsistency"
}

/** A singleton `Inconsitency` with no feedback. */
object Inconsistency extends Inconsistency {
  
  override final val feedback = None
  final val putFeedback = "true" == System.getProperty("InconsistencyWithFeedback")

  /** Returns a new instance of `Inconsistency` with message as feedback,
    * if the flag -DInconsistencyWithFeedback=true is set in the JVM.
    * Else, returns the basic Inconsistency object. */
  final def apply(message: Any): Inconsistency = {
    if(putFeedback)
      new Inconsistency { override final val feedback = message}
    else
      this
  }

  def get: Inconsistency = this
}