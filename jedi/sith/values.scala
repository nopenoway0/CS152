package value

import expression._
import ui._
import scala.collection.mutable.HashMap

trait Value extends java.io.Serializable

case class Boole(val value: Boolean) extends Literal{
	def &&(other: Boole) = Boole(this.value && other.value)
	def ||(other: Boole) = Boole(this.value || other.value)
	override def toString = value.toString
}

object Boole{
	val TRUE = Boole(true)
	val FALSE = Boole(false)
}

case class Number(val value: Double) extends Literal{
	def +(other: Number) = Number(this.value + other.value)
	def *(other: Number) = Number(this.value * other.value)
	def -(other: Number) = Number(this.value - other.value)
	def /(other: Number) = Number(this.value / other.value)
	def <(other: Number) = Boole(this.value < other.value)
	def >(other: Number) = Boole(this.value > other.value)
	def ==(other: Number) = Boole(this.value == other.value)
	def execute() = this.value
	override def toString() = this.value.toString
}

case class Symbol(val operator: String) extends Literal{
	def execute() = this.operator
}

case class Notification(val msg: String) extends Value{
	override def toString = msg
}

object Notification{
	val OK = Notification("ok")
	val DONE = Notification("done")
}

object alu{
	def execute(operator: Identifier, args: List[Value]): Value = {
		operator.name match{
			case "add" => add(args)
			case "mul" => mul(args)
			case "sub" => sub(args)
			case "div" => div(args)
			case "equals" => equals(args)
			case "content" => content(args)
			case "var" => makeVar(args)
			case _ => throw UndefinedException(operator.name)
		}
	}
	// lots of type checking for numbers otherwise throw an exception

	private def makeVar(args:List[Value]): Value = {
		Variable(args.head)
	}

	private def content(args:List[Value]): Value = {
		args.head.asInstanceOf[Variable].content
	}
	private def add(args:List[Value]): Number = {
		var nums = args.filter(_.isInstanceOf[Number])
		if(nums.length != args.length)
			throw new TypeException("Inputs to add must be numbers")
		val nums2 = nums.map(_.asInstanceOf[Number])
		nums2.reduce(_+_)
	}
	private def mul(args:List[Value]): Number = {
		var nums = args.filter(_.isInstanceOf[Number])
		if(nums.length != args.length)
			throw new TypeException("Inputs to add must be numbers")
		val nums2 = nums.map(_.asInstanceOf[Number])
		nums2.reduce(_*_)
	}
	private def sub(args:List[Value]): Number = {
		var nums = args.filter(_.isInstanceOf[Number])
		if(nums.length != args.length)
			throw new TypeException("Inputs to add must be numbers")
		val nums2 = nums.map(_.asInstanceOf[Number])
		nums2.reduce(_-_)
	}
	private def div(args:List[Value]): Number = {
		var nums = args.filter(_.isInstanceOf[Number])
		if(nums.length != args.length)
			throw new TypeException("Inputs to add must be numbers")
		val nums2 = nums.map(_.asInstanceOf[Number])
		nums2.reduce(_/_)
	}
	private def equals(args:List[Value]): Boole = {
		val nums2 = args
		var matched = Boole.TRUE
		var prev = args(0)
		for(x <- args){
			if(x != prev) matched = Boole.FALSE
			prev = x
		}
		matched
	}
}

//class Closure extends Value
class Environment(var extension: Environment = null) extends HashMap[Identifier, Value] with Value{
	override def apply(name: Identifier): Value = {
		if(contains(name)) super.apply(name)
		else if (extension != null) extension(name)
		else throw new UndefinedException(name.name)
	}
}

case class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
   def apply(args: List[Value]): Value = {
   		val localEnv = new Environment(defEnv)
	   	if(params.length != args.length) throw new UndefinedException("Invalid Environment")
	 	for((x,y) <- (params zip args)) localEnv.put(x, y)
	   	var result = body.execute(localEnv)	   	
	   	result
	      //1. create localEnv extending defEnv // for static scope rule
	      //2. bind params to args in localEnv
	      //3. body.execute(localEnv)
   }

   override def toString = "Closure(" + params.toString + body.toString + ")"
}

case class Variable(var content: Value) extends Value{
	def execute() = this.content
}

