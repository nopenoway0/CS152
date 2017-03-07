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

//package amoeba

import scala.collection.mutable.HashMap

object processor {
  
  var currentEnv = new Environment
  var program: Array[String] = null
  var ip = 0
  var ir:Array[String] = null
  val labels = new HashMap[String, Int]
  
  def preProcess(fileName: String) {
    program = io.Source.fromFile(fileName).getLines.toArray
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
      try {
        // load ir with opcode and operands of program(ip) (hint: use split)
        // increment ip
        // skip empty instructions?
        // based on opcode, execute the instruction (hint: use opcode match ...)
        // if opcode = "halt" then halt = true
    } 
    println("bye ... ")
  }

  def main(args: Array[String]): Unit = {
    val programFile = readLine("Enter program name: ")
    preProcess(programFile)
    fetchExecute
  }

}