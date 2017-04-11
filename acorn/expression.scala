package expression

import value._

trait Expression{
	def execute: Value // returns a value
}

class Literal extends Expression with Value{
	def execute = this
}

class Sum(val operand1: Expression, val operand2: Expression) extends Expression{
	def execute = {
		val value1: Value = operand1.execute
		val value2: Value = operand2.execute
		if (!value1.isInstanceOf[Number] || !value2.isInstanceOf[Number])
		  throw new Exception("type mismatch: only numbers can be added")
		else {
		  value1.asInstanceOf[Number] + value2.asInstanceOf[Number]
	}
	}

  	override def toString = "(" + operand1.toString + " + " + operand2.toString + ")"
}

object Sum{
	def apply(operand1: Expression, operand2: Expression) = {
		new Sum(operand1, operand2)
	}
}


class And(val operand1: Expression, val operand2: Expression) extends Expression{
	// Partial on exam. Want short circuit execution
	def execute = {
		val value1: Value = operand1.execute
		val value2: Value = operand2.execute
		if(!value1.isInstanceOf[Boole])
			throw new Exception("type mismatch: only booles can be anded")
		val bool1 = value1.asInstanceOf[Boole]
		if(!bool1.value) bool1
		else {
			if(!value2.isInstanceOf[Boole])
				throw new Exception("type mismatch: only booles can be anded")
			val bool2 = value2.asInstanceOf[Boole]
			bool2
		}
				
	}

	override def toString = "(" + operand1.toString + " AND " + operand2.toString + ")"
}

object And{
	def apply(operand1: Expression, operand2: Expression) = new And(operand1, operand2)
	def test = "test"
}

class Times(val operand1: Expression, val operand2: Expression) extends Expression{
	def execute = {
		val value1 = operand1.execute
		val value2 = operand2.execute
		if (!value1.isInstanceOf[Number] || !value2.isInstanceOf[Number])
		  throw new Exception("type mismatch: only numbers can be added")
		else {
		  val num1 = value1.asInstanceOf[Number]
		  val num2 = value2.asInstanceOf[Number]
		  num1 * num2
	}
	}

  	override def toString = "(" + operand1.toString + " + " + operand2.toString + ")"
}

object Times{
	def apply(operand1: Expression, operand2: Expression) = {
		new Times(operand1, operand2)
	}
}