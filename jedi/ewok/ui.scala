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

	def expression: Parser[Expression] = declaration | funcall// | identifier | literal

	def declaration: Parser[Declaration] = "def"~identifier~"="~expression ^^{
		case "def"~id~"="~exp => Declaration(id, exp)
	}

	def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^{
		case someString=>Identifier(someString)
	}

	def literal: Parser[Literal] = boole | number

	def boole: Parser[Boole] = ("true" | "false") ^^{
		case someString => Boole(someString.toBoolean)
	}

	def number: Parser[Number] = "(\\+|\\-)?[0-9]+(\\.[0-9]+)?".r ^^{
		case someString => Number(someString.toDouble)
	}

	def funcall: Parser[Expression] = (identifier | literal)~opt(operator~funcall) ^^{
		case arg1~None =>{
			arg1
		}

		case arg1~Some(additional) =>{
			var args = List[Expression]()
			val op = additional._1
			val arg2 = additional._2
			args = args :+ arg1
			args = args :+ arg2
			Funcall(op, args)
		}
		case _ =>{
			throw new SyntaxException("RIP")
		}
	}

	def operator: Parser[Identifier] = "\\/|\\*|\\+|\\-".r ^^{
		case "+" => Identifier("add")
		case "*" => Identifier("mul")
		case "/" => Identifier("div")
		case "-" => Identifier("sub")
	}
	/*
	//def expression: Parser[Expression] = number
	//def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")
	//def declaration: Parser[Declaration] = "def"~identifier~"="~expression ^^{
		case "def"~id~"="~exp => {
			Declaration(id, exp)
		}
		case _ => throw new SyntaxException("Error making dec")
	}

	def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^{
		case someString=>Identifier(someString)
	}

	//def sum: Parser[Number] = term~"+"~term ^^{
	//	case exp1~"+"~exp2 => {
	//		println(exp1 + " " + exp2)
	//		Number(0)
	//	}
	//}
/*
	def disjunction: Parser[Expression] = conjuction ~ rep("||" ~ conjuction ) ^^{
		case con ~ Nil => con
		case con ~ cons => Disjunction(con::cons) // adds con to cons beginning of list
	}*/
	//def declaration, conditional, dusjunction, and other parsers
	//def operands: Parser[List[Expression]] = {
	//	case "(" ~ opt(expression~reptition(","~expression))~")"
	//	case "(" ~ Some(exp) ~ Nil ~ ")" => (exp)
	//}

	def literal: Parser[Literal] = number

	def number: Parser[Number] = """"[0-9]*""".r ^^{
		case someString => Number(someString.toDouble)
	}
	*/
}