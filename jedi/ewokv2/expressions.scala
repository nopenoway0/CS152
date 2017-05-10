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

//case class Disjunction(val arg1: Expression, val arg2: List[Expression] = null) extends SpecialForm{
case class Disjunction(val arg1: Expression, val arg2: Expression = null) extends SpecialForm{
	def execute(env: Environment) = {
		val value = arg1.execute(env)
		if(arg2 == null || value == Boole.TRUE) value
		else if(!value.isInstanceOf[Boole]) throw new TypeException("Need Boolean for Disjunction")
		else{
			val value2 = arg2.execute(env)
			if (!value2.isInstanceOf[Boole]) throw new TypeException("Need Boolean for Disjunction")
			else value2.asInstanceOf[Boole] || value2.asInstanceOf[Boole]
			}
		}
	
}

case class Equality(val arg1: Expression, val arg2: Expression) extends SpecialForm{
	def execute(env: Environment) = {
		val value1 = arg1.execute(env)
		val value2 = arg2.execute(env)
		if(value1 == value2) Boole.TRUE
		else Boole.FALSE
	}
}

case class Inequality(val arg1: Expression, val arg2: Expression = null, val operator: Identifier = null) extends SpecialForm{
	def execute(env: Environment) = {
		val value = arg1.execute(env)
		if(arg2 == null) value
		else if (operator == null) throw new SyntaxException("Operator not specifed")
		else{
			val value2 = arg2.execute(env)
			operator.name match{
				case "<" => value.asInstanceOf[Number] < value2.asInstanceOf[Number]
				case ">" => value.asInstanceOf[Number] > value2.asInstanceOf[Number]
			}
		}
	}
}

case class Identifier(val name: String) extends Expression{
	def execute(env: Environment) = env(this)
}
/*
case class Funcall(val operator: Identifier, operands: List[Expression]) extends Expression{
	def execute(env: Environment): Value = {
		var args = List[Value]()
		println("Funciton Call made")
		// add operands.asInstanceOf
		args = operands.map(_.execute(env))
		alu.execute(operator, args)
	}
}*/

case class Conditional(val condition: Expression, val result: Expression, val elseCond:Expression = Boole.FALSE) extends Expression{
	def execute(env: Environment) = {
		if(condition.execute(env) == Boole.TRUE)
			result.execute(env)
		else elseCond.execute(env)
	}
}

case class Product(arg1: Expression, arg2: List[Expression] = null) extends Expression {
   def execute(env: Environment) = 
     if (arg2 == null) arg1.execute(env) else arg1.execute(env).asInstanceOf[Number] * arg2.map(_.execute(env).asInstanceOf[Number]).reduce(_*_)
}

case class Divisor(arg1: Expression, arg2: List[Expression] = null) extends Expression{
   def execute(env: Environment) = 
     if (arg2 == null) arg1.execute(env) else arg1.execute(env).asInstanceOf[Number] / arg2.map(_.execute(env).asInstanceOf[Number]).reduce(_/_)
}

case class Sum(arg1: Expression, arg2: List[Expression] = null) extends Expression {
   def execute(env: Environment) = {
    if (arg2 == null) arg1.execute(env) else arg1.execute(env).asInstanceOf[Number] + arg2.map(_.execute(env).asInstanceOf[Number]).reduce(_+_)
	}
}

case class Sub(arg1: Expression, arg2: List[Expression] = null) extends Expression {
   def execute(env: Environment) = 
     if (arg2 == null) arg1.execute(env) else arg1.execute(env).asInstanceOf[Number] - arg2.map(_.execute(env).asInstanceOf[Number]).reduce(_-_)
}

case class Conjunction(arg1: Expression, arg2: Expression = null) extends Expression{
	def execute(env:Environment) = {
		val value = arg1.execute(env)
		if(arg2 == null || value == Boole.FALSE) value
		else if(!value.isInstanceOf[Boole]) throw new TypeException("Need Boolean for Disjunction")
		else{
			val value2 = arg2.execute(env)
			if(value2.isInstanceOf[Boole]) value.asInstanceOf[Boole] && value2.asInstanceOf[Boole]
			else throw TypeException("Need Boole for Disjunction")
		}
	}
}

case class Block(val locals: List[Expression]) extends SpecialForm {
   def execute(env: Environment) = {
	   	var lastVar = locals(locals.length - 1)
	   	for(x <- locals){
	   		lastVar = x
	 		x.execute(env)
	   	}// Remove last local?
	   	lastVar.execute(env)
	     // 2. for local in locals local.execute(localEnv)
	     // 3. return last one
   }
}


case class Lambda(params:List[Identifier], body:Expression, env:Environment)  extends SpecialForm {
   def execute(env: Environment) = {
      Closure(params, body, env)
   }
}	

case class Funcall(val operator: Expression, val operands: List[Expression]) extends Expression {
   def execute(env: Environment) = {
      val args = operands.map(_.execute(env)) // eager execution
      try {
      	   val fun = operator.execute(env)
           if (!fun.isInstanceOf[Closure]) throw new TypeException("Error")
           fun.asInstanceOf[Closure].apply(args)
      } catch {
        case e: UndefinedException => alu.execute(operator.asInstanceOf[Identifier], args)
      //	case f: Exception => throw new UndefinedException("Error")
      }}
}