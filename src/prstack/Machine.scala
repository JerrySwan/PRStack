package prstack

case class Machine(instructions : List[Instr]) {
  
  var ix : Int = 0
  var stack : List[Term] = List()
  
  def execute( i : Instr ) : Unit = i match {
    case I0 => ix += 1
    case I1 => (ix % 6) match {
      case 0 => stack match {
        case (s1 :: s2 :: ss) => { stack = s2(s1) :: ss }
        case _                => { }
      }
      case 1 => { stack = K      :: stack ; ix = 0}
      case 2 => { stack = S      :: stack ; ix = 0}
      case 3 => { stack = Num(0) :: stack ; ix = 0}
      case 4 => { stack = Succ   :: stack ; ix = 0}
      case _ => { stack = Iter   :: stack ; ix = 0}
    }
  }
  
  def run : Unit = for(i <- instructions) { execute(i) }
  
}