package ui
//
// package system?
import scala.util.parsing.combinator._
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
class TypeException(gripe: String) extends JediException(gripe){
	def msg = gripe
}

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

	def term: Parser[Expression] =  literal | identifier | paren

	def expression: Parser[Expression] =  declaration | conditional | equality | inequality |  funcall | sum | product | term//declaration | sum | product//|product/*inequality | equality | sum | product | funcall*/

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

	def identifier: Parser[Identifier] = """[a-zA-Z]+[a-zA-Z0-9]*""".r ^^{
		case someString=> Identifier(someString)
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
			throw new SyntaxException("Empty Function Call: " + id)
		}
		case id~"("~Some(exp)~")"=>{
			Funcall(id.asInstanceOf[Identifier], exp)
		}
		case _ => throw new SyntaxException("Error")
	}

	def product: Parser[Expression] = /*(funcall|term)*/term~opt(("*" | "/")~product)^^{
		case call1~None =>{
			Product(call1)
		}
		case term~Some(terms)=>{
			var args = List[Expression]()
			args = args :+ terms._2
			if(terms._1 == "/") Divisor(term, args)
			else Product(term, args)
		}
		case _ => throw new SyntaxException("Error")
	}

	def sum: Parser[Expression] = product~rep(("+" | "-")~(product)) ^^{
		case call1~list =>list.foldLeft(call1){
			case (x, "+" ~ y)=>{
				var arg = List[Expression]()
				arg = arg :+ y
				Sum(x, arg)
			}
			case (x, "-" ~ y)=>{
				var arg = List[Expression]()
				arg = arg :+ y
				if(arg.size == 0) throw new SyntaxException("Need Operand")
				Sub(x,arg)
			}
		}
		//case _ => throw new SyntaxException("Error")
	}

	def paren: Parser[Expression] = "("~sum~")" ^^{
		case "("~s1~")" =>{
			s1
		}
		case _ => throw new SyntaxException("invalid paren + param")
	}

	def conditional: Parser[Conditional] = "if"~"("~(equality | inequality)~")"~expression~opt("else"~expression) ^^{
		case "if"~"("~exp1~")"~exp2~None=>{
			Conditional(exp1, exp2)
		}
		case "if"~"("~exp1~")"~exp2~Some(exp3)=>{
			Conditional(exp1, exp2, exp3._2)
		}
		case _ => throw new SyntaxException("Error")
	}

	def equality: Parser[Equality] = (inequality | term)~"=="~(inequality | term) ^^{
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