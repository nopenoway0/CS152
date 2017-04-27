package value

import expression._
import ui._
import scala.collection.mutable.HashMap

trait Value extends java.io.Serializable

case class Boole(val value: Boolean) extends Literal{
	def &&(other: Boole) = Boole(this.value && other.value)
	def ||(other: Boole) = Boole(this.value || other.value)
}

case class Number(val value: Double) extends Literal{
	def +(other: Number) = Number(this.value + other.value)
	def *(other: Number) = Number(this.value * other.value)
	def -(other: Number) = Number(this.value - other.value)
	def /(other: Number) = Number(this.value / other.value)
	def <(other: Number) = Boole(this.value < other.value)
	def ==(other: Number) = Boole(this.value == other.value)
	def execute() = this.value
}


case class Notification(val msg: String) extends Value{
	override def toString = msg
}

object Notification{
	def apply(msg: String) = new Notification(msg)
	val OK = Notification("ok")
	val DONE = Notification("done")

}

object alu{
	def execute(operator: Identifier, args: List[Value]): Value = {
		operator.name match{
			case "add" => add(args)
			//etc.
			case _ => UndefinedEx(operator.name)
		}
	}
	// lots of type checking for numbers otherwise throw an exception
	private def add(args:List[Value]): Number = {
		// function goes here
	}
}

//class Closure extends Value
class Environment(var extension: Environment = null) extends HashMap[Identifier, Value] with Value{
	override def apply(name: Identifier): Value = {
		if(contains(name)) super.apply(name)
		else if (extension != null) extension(name)
		else throw new UndefinedException(name)
	}
}