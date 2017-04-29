package expression

import value._
import ui._
trait Expression{
	def execute(env: Environment): Value
}

trait SpecialForm extends Expression

trait Literal extends Expression with Value{
	def execute(env: Environment) = this
}

case class Declaration(val name: Identifier, val body: Expression) extends SpecialForm{
	def execute(env: Environment): Value = {
		val result = body.execute(env)
		env.put(name, result)
		Notification.OK
	}
}

case class Disjunction(val exps:List[Expression]) extends SpecialForm{
	def execute(env: Environment) = {
		more = true
		for(exp <- exps if more){
			val arg = exp.execute(env)
			if(!arg.isInstanceOf[Boole]) throw new TypeException("Must be Boole value")
			// add other shortcircuiting and define type exception
			val b = arg.asInstanceOf[Boole]
			if(b.value){
				result = Boole.TRUE
				more = false
			}
		}
	}
	result
}

case class Identifier(val name: String) extends Expression{
	def execute(env: Environment) = env(this)
}

class Funcall(val operator: Identifier, operands: List[Expression]) extends Expression{
	def execute(env: Environment): Value = {
		var args = List[Value]()
		// add operands.asInstanceOf
		val args = operands.map(_.execute(env))
		alu.execute(operator, args)
	}
}