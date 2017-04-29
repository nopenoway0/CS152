package ui
//
// package system?
//import scala.util.parsing.combinator._ // for the parser. Add external jars. Build into make file. link to the jar library
import expression._
import value._

class JediException(val gripe: String = "Jedi error ") extends Exception(gripe)

class UndefinedException(name: Identifier) extends JediException("Unidentified identifier: " + name)

// Incomplete
class TypeException extends JediException("Type Exception")

// Incomplete
class SyntaxException extends JediException("Syntax Exception")


/*
class EwokParsers extends RegexParsers {

	def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

	def declaration: Parse[Declaration]  = "def ~ identifier ~ "=" ~ expression ^^{
		case "def" ~id~"="~exp => Declaration(id, exp)
	}

	def identifier: Parser[Identifier] = """[a-zA-Z][a-zA0-9]*""".r ^^{
		case someString=>Identifier(someString)
	}

	def disjunction: Parser[Expression] = conjuction ~ rep("||" ~ conjuction ) ^^{
		case con ~ Nil => con
		case con ~ cons => Disjunction(con::cons) // adds con to cons beginning of list
	}
	//def declaration, conditional, dusjunction, and other parsers
	def operands: Parser[List[Expression]] = {
		case "(" ~ opt(expression~reptition(","~expression))~")"
		case "(" ~ Some(exp) ~ Nil ~ ")" => (exp)
	}
}
*/

/*
// Console object
// 
object console{
   val parsers = new EwokParsers // for now
   val globalEnv = new Environment

   def execute(cmmd: String): String = {
      val tree = parsers.parseAll(parsers.expression, cmmd)
      tree match {
         case t: parsers.Failure => throw new SyntaxException(t)
         case _ => {
            val exp = tree.get  // get the expression from the tree
            val result = exp.execute(globalEnv)  // execute the expression
            result.toString  // return string representation of result
         }
      }
   }

   /*
	while(more){
		try {// read/execute/print
			print("->")
			cmmd = 
			if(cmmd == "quit") more = false
			else println(execute(cmmd))
	}
	catch {
		case 	e: SyntaxException =>{
			println(e.msg)
			println(e.result.msg)
			println("line # = " + e.result.next.pos.line)
			println("column # = " + e.result.next.pos.column)
			println("token = " + e.result.next.first_)
		}
		//case 	e:
		case 	e:JediException => {
			println(e)
		} 
		case 	e: Exception => {
			println(e)
			more = false
		}
	}*/
}
*/