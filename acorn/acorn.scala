//package system

import value._
import expression._

object testAcorn {
  def execute(exp: Expression) {
    try {
      val value = exp.execute
      println(exp.toString + " = " + value)
    } catch {
      case e: Exception => println(e)
    }
  }
    
  def main(args: Array[String]) {
    execute(Sum(Number(3.1), Number(4.2)))
    //execute(And(Boole(true), Boole.TRUE))
    //execute(Sum(Number(3.1), Boole.FALSE))
    //execute(Sum(Sum(Number(3.1), Number(4.2)), Sum(Number(3.5), Number(2.8))))
  }
}