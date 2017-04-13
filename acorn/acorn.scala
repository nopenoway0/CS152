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

    execute(And(Boole(true), Boole.TRUE))

    execute(Sum(Number(3.1), Boole.FALSE))

    execute(And(Boole.FALSE, Number(3.1)))

    execute(Sum(Sum(Number(3.1), Number(4.2)), Sum(Number(3.5), Number(2.8))))
    execute(Sum(Times(Number(9), Number(2)), Divide(Number(5), Number(2))))

    execute(Equal(Times(Number(3),Number(4)), Times(Number(4), Number(3))))
    execute(Equal(Divide(Number(3),Number(4)), Divide(Number(4), Number(3))))

    execute(Divide(Number(3), Number(1.2)))
    execute(Divide(Number(100),Number(.4)))

    execute(Times(Number(25), Number(2)))

    execute(Subtract(Number(8), Number(-11)))

    execute(Or(Boole.FALSE, Boole.TRUE))
    execute(Or(Boole.TRUE, Boole.TRUE))
    execute(Or(Boole.FALSE, Boole.FALSE))

    execute(Not(Boole.FALSE))
    execute(Not(Boole.TRUE))

    execute(Less(Times(Number(10), Number(10)), Divide(Times(Number(10),Number(10)), Number(100))))

    execute(Less(Divide(Times(Number(10),Number(10)), Number(100)), Times(Number(10), Number(10))))
  }
}