package value

import expression._

import scala.collection.mutable.HashMap

trait Value extends java.io.Serializable

//class Boole extends Literal

//class Number extends Literal

//class Closure extends Value

class Environment(var extension: Environment = null) extends HashMap[Identifier, Value] with Value{
	override def apply(name: Identifier): Value = {
		if(contains(name)) super.apply(name)
		else if (extension != null) extension(name)
		else throw new Exception("undefined variable: " + name)
	}
}