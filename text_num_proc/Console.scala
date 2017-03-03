import java.util.Scanner

object console{
	def execute(cmmd: String) = {
		var tokens = cmmd.trim.split("\\s+")
		tokens(0) match{
			case "quit" => ""
			case "help" => "commands: add, mul, sub, div, quit, help"
			case "add" => (tokens(1).toDouble + tokens(2).toDouble).toString
			case "mult" => (tokens(1).toDouble * tokens(2).toDouble).toString
			case "sub" => (tokens(1).toDouble * tokens(2).toDouble).toString
			case "div" => (tokens(1).toDouble / tokens(2).toDouble).toString
			case default => throw new Exception("Invalid Command " + tokens(0))
		}
	}


	def repl = {
		var in:Scanner = null
		var continue = true
		var result:String = null
		while(continue){
			print("->")
			in = new Scanner(System.in)
			try{
			result = execute(in.nextLine)
			if(result.length == 0) continue = false
			else println(result)
			}catch{
				case e: Exception => println(e.getMessage)
			}
		}
	}

	def main(args:Array[String]) = {
		repl
	}
}