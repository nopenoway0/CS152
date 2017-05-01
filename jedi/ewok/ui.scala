package ui
//
// package system?
import scala.util.parsing.combinator._ // for the parser. Add external jars. Build into make file. link to the jar library
import expression._
import value._

class JediException(gripe: String) extends Exception(gripe)

object JediException{
	def apply(gripe: String) = new JediException(gripe)
}

class UndefinedException(name: String) extends JediException("Unidentified identifier: " + name)

object UndefinedException{
	def apply(name: String) = new UndefinedException(name)
}

// Incomplete
class TypeException(gripe: String) extends JediException(gripe)

object TypeException{
	def apply(gripe: String) = new TypeException(gripe)
}

// Incomplete
class SyntaxException(gripe: String) extends JediException(gripe){
	def msg = gripe
}

object SyntaxException{
	def apply(gripe: String) = new SyntaxException(gripe)
}


class EwokParsers extends RegexParsers {
	def term: Parser[Expression] =  literal | identifier | expression

	def expression: Parser[Expression] = declaration | inequality | equality | sum | product | funcall// | funcall | failure("Error")

	def literal: Parser[Literal] = boole | number

	def boole: Parser[Boole] = ("true" | "false") ^^{
		case someString => Boole(someString.toBoolean)
	}

	def number: Parser[Number] = "(\\+|\\-)?[0-9]+(\\.[0-9]+)?".r ^^{
		case someString => Number(someString.toDouble)
	}

	def declaration: Parser[Declaration] = "def"~identifier~"="~expression ^^{
		case "def"~id~"="~exp => Declaration(id, exp)
	}

	def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^{
		case someString=>Identifier(someString)
	}

	def operands: Parser[List[Expression]] = expression ~ opt((","~operands)) ^^{
		case exp1~None =>{
			var args = List[Expression]()
			args = args :+ exp1
			args
		}
		case exp1~Some(rest) =>{
			var args = List[Expression]()
			args = args :+ exp1
			args = args ++ rest._2
			args
		}
		case _ => throw new SyntaxException("Error")
	}

	def funcall: Parser[Expression] = term~"("~opt(operands)~")" ^^{
		case id~"("~None~")"=>{
			throw new SyntaxException("Error")
		}
		case id~"("~Some(exp)~")"=>{
			Funcall(id.asInstanceOf[Identifier], exp)
		}
		case _ => throw new SyntaxException("Error")
	}

	def product: Parser[Expression] = (funcall|term)~opt(("*" | "/")~product)^^{
		case call1~None =>{
			Product(call1)
		}
		case call1~Some(m)=>{
			var arg = List[Expression]()
			arg = arg :+ m._2
			if(m._1 == "*") Product(call1, arg)
			else Divisor(call1, arg)
			//Product(call1, call2)
		}
		case _ => throw new SyntaxException("Error")
	}

	def sum: Parser[Expression] = product~rep(("+" | "-")~product) ^^{
		case call1~list =>list.foldLeft(call1){
			case (x, "+" ~ y)=>{
				var arg = List[Expression]()
				arg = arg :+ y
				Sum(x, arg)
			}
			case (x, "-" ~ y)=>{
				var arg = List[Expression]()
				arg = arg :+ y
				Sub(x,arg)
			}
		}
		//case _ => throw new SyntaxException("Error")
	}
/*
	def conditional: Parser[Conditional] = "if"~"("~expression~")"~expression~opt("else"~expression) ^^{
		case "if"~"("~exp1~")"~exp2~None=>{
			Conditional(exp1, exp2)
		}
		case "if"~"("~exp1~")"~exp2~Some(exp3)=>{
			Conditional(exp1, exp2, exp3._2)
		}
		case _ => throw new SyntaxException("Error")
	}
*/
	def equality: Parser[Equality] = inequality~"=="~inequality ^^{
		case arg1~"=="~arg2=>{
			Equality(arg1, arg2)
		}
	}


	def inequality: Parser[Inequality] = sum~("<" | ">")~sum ^^{
		case s1~"<"~s2=>{
			Inequality(s1, s2, Identifier("<"))
		}
		case s1~">"~s2=>{
			Inequality(s1, s2, Identifier(">"))
		}		case _ => throw new SyntaxException("Error")
	}
/*
	def disjunction: Parser[Expression] = conjuction ~ rep("||" ~ conjuction ) ^^{
		case con ~ Nil => con
		case con ~ cons => Disjunction(con::cons) // adds con to cons beginning of list
	}*/
}