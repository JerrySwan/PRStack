package prstack

sealed trait Type {
  def atomic : Boolean = true
  
  def ~~>(that : Type) : Type = Fun(this,that)
}

case object Nat extends Type {
  override def toString : String = "Nat"
}

case class Var(num : Int) extends Type {
  override def toString : String = "x" + num
}

case class Fun(source : Type, target : Type) extends Type {
  override def atomic : Boolean = false
  
  override def toString : String = {
    val sourceFormat : String = if (source.atomic) source.toString else "(" + source.toString + ")"
    val targetFormat : String = if (target.atomic) target.toString else "(" + target.toString + ")"
    sourceFormat + " ~~> " + targetFormat
  }
  
}

