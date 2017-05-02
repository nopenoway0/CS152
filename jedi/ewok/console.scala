import ui._
import value._
import expression._
import java.util.Scanner
import scala.util.parsing.combinator._
// Console object
// 
object console{
   val parsers = new EwokParsers // for now
   val globalEnv = new Environment
   //Declaration(Identifier("+"), Identifier("+"))

   def execute(cmmd: String): String = {
      val tree = parsers.parseAll(parsers.expression, cmmd)
      tree match {
         case t: parsers.Failure => throw new SyntaxException(t.msg)
         case _ => { 
            val exp = tree.get  // get the expression from the tree
            val result = exp.execute(globalEnv)  // execute the expression
            result.toString  // return string representation of result
         }
      }
   }
   
    def repl {
      // declare locals
      var more = true
      var cmmd = ""
      //Declaration(Identifier("+"), Symbol("add")).execute(globalEnv)
      while(more) {
         try {
            // read/execute/print
            print("-> ")
            cmmd = scala.io.StdIn.readLine()
            //cmmd = cmmd.trim
            if(cmmd == "quit"){
               more = false
               cmmd = ""
            }
            // handle meta-commands
            if(cmmd.size > 0) println(execute(cmmd))
         } 
         catch {
            case t: TypeException => println(t.msg)
            case e: SyntaxException => {
               println(e.msg)
               //println(e.result.msg)
               //println("line # = " + e.result.next.pos.line)
               //println("column # = " + e.result.next.pos.column)
               //println("token = " + e.result.next.first)
            }
            case u: UndefinedException => {
               println("UndefinedException")
            }
            case j: Exception => println("Fatal Error")
            // handle other types of exceptions
         } finally {
            Console.flush 
         }
      }
   }
    
   def main(args: Array[String]): Unit = { repl }
}