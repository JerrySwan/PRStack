package prstack

sealed trait Instr { }
case object I0 extends Instr {
  override def toString : String = "0"
}
case object I1 extends Instr {
  override def toString : String = "1"
}
