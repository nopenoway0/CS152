/*
CHANGE NAME TO PROCESSOR.SCALA

Requirements:
comment: this is a comment, anything on this line is ignored
label: string following this is considered a label
def x 2 defines the x variable as int 2
load var src change the value currently in var, loads the integer or variable into the var
opcode var src1 src2 does appropriate operation to the two sources stored in var
	opcodes: add, mult, div, sub, and, or, equal, not
	not used to define boolean, any number greater than 0 stores 1 into the first var
goto label, sets ip to label in table
if if var is not equal to 0 go to designated label
printmsg anything following printed to the console
read var stores input into var
print response prints var to console
0 is false other integer is true
or src1 != 0 || src2 != 0
 */

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class Variable(var content: Int = 0)

class Environment(var extension: Environment = null) extends HashMap[String, Variable] {
  
  put("return", new Variable(0))
  
  override def apply(name: String) = {
    if (contains(name)) super.apply(name)
    else if (extension != null) extension(name)
    else throw new Exception("undefined variable: " + name)
  }
}

object processor {
  
  var currentEnv = new Environment
  var program: Array[String] = null
  var ip = 0
  var ir:Array[String] = null
  var envStack: Stack[Environment] = Stack()
  val labels = new HashMap[String, Int]
  
  def preProcess(fileName: String) {

    // useless just for testing
    try{
      program = io.Source.fromFile(fileName).getLines.toArray
    }catch{
      case e: Exception => println("\n" + e.getMessage)
      throw new Exception(e.getMessage())
    }

    for(i <- 0 until program.length) {
      if (!program(i).isEmpty) {
         ir = program(i).split("\\s+")
         if (ir(0) == "label:") labels.put(ir(1), i)
      }
    }
  }
  
  // useful for converting variables or literals to ints:
  def get(term: String): Int = if (term.matches("\\d+")) term.toInt else currentEnv(term).content
  
  
  def fetchExecute() {
    var halt = false
    while(!halt) {
     // try {
        // load ir with opcode and operands of program(ip) (hint: use split)
        ir = program(ip).split("\\s+")
        // increment ip
        ip += 1
        // skip empty instructions? comments need not be read, and labels are already stored in pre process
        if(ir(0).size > 0 && ir(0) != "comment:" && ir(0) != "label:"){
        // based on opcode, execute the instruction (hint: use opcode match ...)
        // if opcode = "halt" then halt = true
        // TODO: Add check for None or Some class add check for 0
          ir(0) match{
            case "if" => if(get(ir(1)) != 0) ip = labels(ir(2))
            case "equal" => if(get(ir(2)) == get(ir(3))) currentEnv(ir(1)).content = 1
                            else currentEnv(ir(1)).content = 0
            case "add" =>   currentEnv(ir(1)).content = get(ir(2)) + get(ir(3))
            case "and" => if(get(ir(2)) != 0 && get(ir(3)) != 0) currentEnv(ir(1)).content = 1
                          else currentEnv(ir(1)).content = 0
            case "or"  => if(get(ir(2)) != 0 || get(ir(3)) != 0) currentEnv(ir(1)).content = 1
                          else currentEnv(ir(1)).content = 0
            case "mul" =>   currentEnv(ir(1)).content = get(ir(2)) * get(ir(3))
            case "div" =>   currentEnv(ir(1)).content = get(ir(2)) / get(ir(3))
            case "sub" =>   currentEnv(ir(1)).content = get(ir(2)) - get(ir(3))
            case "not" =>   if(get(ir(2)) == 0) currentEnv(ir(1)).content = 1
                            else currentEnv(ir(1)).content = 0
            case "call" =>  envStack.push(currentEnv)
                            currentEnv = new Environment
                            for(x <- 2 until ir.size) currentEnv.put("arg" + (x - 2).toString, envStack.top(ir(x)))
                            currentEnv.put("rp", new Variable(ip))
                            ip = labels(ir(1))
            case "read" =>  currentEnv(ir(1)).content = readLine.toInt
            case "printmsg" =>  println 
                                for(x <- 1 until ir.size) print(ir(x) + " ")
            case "print" => print(get(ir(1)))
            case "load" => currentEnv(ir(1)).content = get(ir(2))
            case "def" => currentEnv(ir(1)).content = ir(2).toInt
            case "halt" => halt = true
            case "goto" =>  ip = labels(ir(1))
            case "return" =>  ip = get("rp")
                              envStack.top.put("return", new Variable(get(ir(1))))
                              currentEnv = envStack.pop
            case default => println("unrecognized opcode: " + ir(0))
          }
        }
 //     } 
    }
    println("bye ... ")
  }
  def main(args: Array[String]): Unit = {
    val programFile = readLine("Enter program name: ")
    preProcess(programFile)
    fetchExecute
    
    //println(labels)
    //println(currentEnv)
  }

}