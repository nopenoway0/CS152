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

case class Identifier(val name: String) extends Expression{
	def execute(env: Environment) = env(this)
}