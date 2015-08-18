package prstack

object Examples {
  
  
  def main(args : Array[String]) : Unit = {
    
    val testinstr : List[Instr] = List( I0,I0,I1          // S
                                      , I0,I1             // K
                                      , I1                // apply
                                      , I0,I1             // K
                                      , I1                // apply
                                      , I0,I0,I0,I1       // 0
                                      , I1                // apply
                                      )
                                      
    // hint: comment out last two lines of testinstr for the id function
    
    println("Executed program: " + (testinstr mkString ""))                              
    
    val checker = TypeInferencer(testinstr)
    println("Inferred type: " + checker.check.getOrElse("NOT TYPE CORRECT"))
    
    val machine = Machine(testinstr)
    machine.run
    
    println("output: " + machine.stack.headOption.getOrElse("NO OUTPUT"))
  }
  
}