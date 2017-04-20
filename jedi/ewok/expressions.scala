package expression

import value._

trait Expression{
	def execute(env: Environment): Value
}

trait SpecialForm extends Expression

trait Literal extends Expression with Value

// Change from abstract
class Identifier(val name: String) extends Expression{
	def execute(env: Environment) = env(this)
}