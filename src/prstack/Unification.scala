package prstack

object Unification {
  
  type Unifier = Map[Int,Type]
  
  // TODO: implement occurs check
  def occurs( ix : Int, tpe : Type ) : Boolean = false
  
  /**
   * Applies a unifier to a type.
   */
  def applyUnifier( u : Unifier, on : Type ) : Type = on match {
    case Var(v)   => u.getOrElse(v, Var(v))
    case Fun(a,b) => Fun(applyUnifier(u,a),applyUnifier(u,b))
    case Nat      => Nat
  }
  
  def findUnifier( t1 : Type, t2 : Type ) : Option[Unifier] = (t1,t2) match {
    case (Var(x), t)              => if (Var(x) != t && occurs(x,t)) None else Some( Map(x -> t) )
    case (t, Var(y))              => if (Var(y) != t && occurs(y,t)) None else Some( Map(y -> t) )
    case (Fun(a1,a2), Fun(b1,b2)) => findUnifiers( List(a1 -> b1, a2 -> b2) )
    case _                        => None
  }
  
  def composeUnifiers( first : Unifier, second : Unifier ) : Unifier = {
    val keys = first.keySet union second.keySet
    keys.map( i => i -> applyUnifier(second,applyUnifier(first,Var(i))) ).toMap
  }
  
  def findUnifiers( equations : List[(Type,Type)] ) : Option[Unifier] = equations match {
    case Nil              => Some( Map() )
    case ((x1,x2) :: xs)  => for {
      tail <- findUnifiers(xs)
      head <- findUnifier( applyUnifier(tail,x1), applyUnifier(tail,x2) )
    } yield composeUnifiers(tail,head) //head ++ tail
  }
  
}