package prstack

sealed trait Term {
  def apply(to : Term) : Term
}
case object K    extends Term {
  override def apply(e : Term) : Term = Kp1(e)
}
case object S    extends Term {
  override def apply(e : Term) : Term = Sp1(e)
}
case object Succ extends Term {
  override def apply(n : Term) : Term = n match {
    case Num(v) => Num(v+1)
    case elze   => elze
  }
}
case object Iter extends Term {
  override def apply(z : Term) : Term = Iterp1(z)
}

case class Kp1(e : Term) extends Term {
  override def apply(f : Term) : Term = e
}
case class Sp1(e : Term) extends Term {
  override def apply(f : Term) : Term = Sp2(e,f)
}
case class Sp2(e : Term, f : Term) extends Term {
  override def apply(g : Term) : Term = e(g)(f(g))
}
case class Iterp1(z : Term) extends Term {
  override def apply(s : Term) : Term = Iterp2(z,s)
}
case class Iterp2(z : Term, s : Term) extends Term {
  override def apply(n : Term) : Term = n match {
    case Num(0) => z
    case Num(v) => s(Num(v-1))
    case elze   => elze
  }
}
case class Num(value : Int) extends Term {
  override def toString : String = value.toString
  override def apply(to : Term) : Term = to
}


