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
 */

import scala.collection.mutable.HashMap
import amoeba.Environment
import amoeba.Variable

object processor {
  
  var currentEnv = new Environment
  var program: Array[String] = null
  var ip = 0
  var ir:Array[String] = null
  val labels = new HashMap[String, Int]
  
  def preProcess(fileName: String) {

    // useless just for testing
    try{
      program = io.Source.fromFile(fileName).getLines.toArray
    }catch{
      case e: Exception => println("\n" + e.getMessage())
      throw new Exception(e.getMessage())
    }

    for(i <- 0 until program.length) {
      if (!program(i).isEmpty()) {
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
            case "equal" => if(get(ir(2)) == get(ir(3))) currentEnv.put(ir(1), new Variable(1))
                            else currentEnv.put(ir(1), new Variable(0))
            case "add" =>   currentEnv.put(ir(1), new Variable(get(ir(2)) + get(ir(3))))
            case "read" =>  currentEnv.put(ir(1), new Variable(readLine.toInt))
            case "printmsg" =>  println 
                                for(x <- 1 until ir.size) print(ir(x) + " ")
            case "print" => print(get(ir(1)))
            case "load" => currentEnv.put(ir(1), new Variable(ir(2).toInt)) // TODO: check if variable exists
            case "def" => currentEnv.put(ir(1), new Variable(ir(2).toInt))
            case "halt" => halt = true
            case "goto" =>  ip = labels(ir(1))
            case default => println("unrecognized opcode: " + ir(0))
          }
        }
 //     } 
    }
    println("bye ... ")
  }
  def main(args: Array[String]): Unit = {
    //val programFile = readLine("Enter program name: ")
    val programFile = "triangle"
    preProcess(programFile)
    fetchExecute
    
    //println(labels)
    //println(currentEnv)
  }

}