package prstack

case class TypeInferencer(instructions : List[Instr]) {
  
  var ix : Int = 0
  var stack : List[Type] = List()
  
  private val x0 : Type = Var(0)
  private val x1 : Type = Var(1)
  private val x2 : Type = Var(2)
  
  def execute( i : Instr ) : Boolean = i match {
    case I0 => { ix += 1; true }
    case I1 => (ix % 6) match {
      case 0 => stack match {
        case (s1 :: Fun(s21,s22) :: ss) => Unification.findUnifier(s1, s21) match {
          case None    => false
          case Some(u) => {
            val s2 = Unification.applyUnifier(u, s22)
            stack = s2 :: ss
            ix = 0
            true
          }
        }
        case _                => false // application on empty programs is not type-correct
      }
      case 1 => {
        stack = x0 ~~> (x1 ~~> x0) :: stack
        ix = 0
        true
      }
      case 2 => {
        stack = (x2 ~~> (x1 ~~> x0)) ~~> ((x2 ~~> x1) ~~> (x2 ~~> x0)) :: stack
        ix = 0
        true
      }
      case 3 => {
        stack = Nat :: stack
        ix = 0
        true
      }
      case 4 => {
        stack = Nat ~~> Nat :: stack
        ix = 0
        true
      }
      case _ => {
        stack = Nat ~~> ((Nat ~~> Nat) ~~> (Nat ~~> Nat)) :: stack
        ix = 0
        true
      }
    }
  }
  
  def check : Option[Type] = {
    val passed : Boolean = instructions.map(execute).min
    if (!passed) None else stack.headOption
  }
  
}
