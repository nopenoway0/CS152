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

class UndefinedException(name: String) extends JediException("Unidentified identifier: " + name){
	override def toString = "Undefined Identifier: " + name
}

object UndefinedException{
	def apply(name: String) = new UndefinedException(name)
}

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

	def expression: Parser[Expression] =  declaration | conditional | disjunction | conjunction | equality | inequality | sum | product | funcall | term | failure//declaration | sum | product//|product/*inequality | equality | sum | product | funcall*/

	def literal: Parser[Literal] = boole | number

	def boole: Parser[Boole] = ("true" | "false") ^^{
		case someString => Boole(someString.toBoolean)
	}

	def number: Parser[Number] = "(\\+|\\-)?[0-9]+(\\.[0-9]+)?".r ^^{
		case someString => Number(someString.toDouble)
	}

	def declaration: Parser[Declaration] = "def"~identifier~"="~expression ^^{
		case "def"~id~"="~exp => Declaration(id, exp)
		case unknown => throw new SyntaxException("Invalid Declaration")
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
		case unknown => throw new SyntaxException("Error")
	}

	def funcall: Parser[Expression] = term~"("~opt(operands)~")" ^^{
		case id~"("~None~")"=>{
			throw new SyntaxException("Empty Function Call: " + id)
		}
		case id~"("~Some(exp)~")"=>{
			Funcall(id.asInstanceOf[Identifier], exp)
		}
		case unknown => throw new SyntaxException("Error")
	}

	def product: Parser[Expression] = (funcall|term)~opt(("*" | "/")~product)^^{
		case call1~None =>{
			Product(call1)
		}
		case term~Some(terms)=>{
			var args = List[Expression]()
			args = args :+ terms._2
			if(args.size == 0) throw new SyntaxException("Need operands")
			if(terms._1 == "/") Divisor(term, args)
			else Product(term, args)
		}
		case unknown => throw new SyntaxException("Error")
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
			case unknown => throw new SyntaxException("Need operand")
		}
		case unknown => throw new SyntaxException("Error")
	}

	def paren: Parser[Expression] = "("~sum~")" ^^{
		case "("~s1~")" =>{
			s1
		}
		case unknown => throw new SyntaxException("invalid paren + param")
	}

	def conditional: Parser[Conditional] = "if"~"("~(conjunction)~")"~expression~opt("else"~expression) ^^{
		case "if"~"("~exp1~")"~exp2~None=>{
			Conditional(exp1, exp2)
		}
		case "if"~"("~exp1~")"~exp2~Some(exp3)=>{
			Conditional(exp1, exp2, exp3._2)
		}
		case unknown => throw new SyntaxException("Error")
	}

	def equality: Parser[Equality] = (inequality | term)~"=="~(inequality | term) ^^{
		case arg1~"=="~arg2=>{
			Equality(arg1, arg2)
		}
	}

	def inequality: Parser[Inequality] = sum~opt(("<" | ">")~sum) ^^{
		case s1~Some(content)=>{
			Inequality(s1, content._2, Identifier(content._1))
		}
		case s1~None=>{
			Inequality(s1)
		}
		case unknown => throw new SyntaxException("Error")
	}
	def disjunction: Parser[Expression] = conjunction ~opt("||"~(disjunction)) ^^{
		case tree1 ~ None => Disjunction(tree1)
		case tree1 ~ Some("||" ~ tree2) => Disjunction(tree1, tree2)
		case unknown => throw new SyntaxException("Error")
	}
	def conjunction: Parser[Expression] = (equality | inequality) ~ opt(("&&") ~ (conjunction)) ^^{
		case tree1 ~ None => Conjunction(tree1)
		case tree1 ~ Some("&&" ~ tree2) => Conjunction(tree1, tree2)
		case unknown => throw new SyntaxException("Error")
	}

	def failure: Parser[Expression] = ".+".r ^^{
		throw new SyntaxException("Invalid Command")
	}
}

class WookieParsers extends EwokParsers {

	override def term: Parser[Expression] =  lambda |block | literal | identifier | paren

	override def expression: Parser[Expression] = declaration | conditional | disjunction | conjunction | equality | inequality | sum | product | funcall | term | failure//declaration | sum | product//|product/*inequality | equality | sum | product | funcall*/

	def block: Parser[Expression] = "{" ~> expression ~ rep(";"~>expression)<~"}" ^^{
   		case exp1~Nil=>{
   			Block(List(exp1))
   		}
   		case exp1~exp2 =>{
   			var args = List[Expression]()
   			args = args :+ exp1
   			for(x <- exp2) args = args :+ x
   			Block(args)
   		}

   		case _ => throw new Exception("Error")
	}
	def parameters: Parser[List[Identifier]] = "("~>identifier~rep(","~>identifier)<~")" ^^{
		case id1~id2=>{
			var params = List[Identifier]()
			params = params :+ id1
			for(x <- id2) params = params :+ x
			params
		}
	}
	def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^{
		case "lambda"~params~expressions =>{
			Lambda(params, expressions)
		}
		case _ => throw SyntaxException("incorrect syntax")
	}
}