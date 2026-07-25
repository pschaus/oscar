package oscar.cp.core

sealed trait CPPropagStrength

object CPPropagStrength {
  case object Weak extends CPPropagStrength
  case object Medium extends CPPropagStrength
  case object Strong extends CPPropagStrength
  case object Automatic extends CPPropagStrength
  val values: Array[CPPropagStrength] = Array(Weak, Medium, Strong, Automatic)
}
