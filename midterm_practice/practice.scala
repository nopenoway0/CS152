object sec2{
	def allPass[T](someArray:Array[T], someTest:T=>Boolean): Boolean = {
		def helper(index: Int, result: Boolean): Boolean = if(index >= someArray.length) result
			else helper(index + 1, result && someTest(someArray(index)))
			helper(0, true)
	}

	def isEven(arg: Int) = if(arg % 2 == 0) true else false


	val parseInt = pipe((s:String) => s.toInt, (s:String) => 0)

	def pipe[T,S](f: T=>S, g: T=>S): T=>S = {
		def h(in: T): S = {
			try{
				f(in)
			}
			catch{
				case e: Exception => g(in)
			}
		}
		h _
	}

	val int_list = Array(1,2,3,4,5,6)
	val even_list = Array(2,4,6,8,24,200)
	def main(args:Array[String]) = {
		println("Test: " + allPass(int_list, isEven _))
		println("Test: " + allPass(even_list, isEven _))
		println(parseInt("424"))
		println(parseInt("3x3"))
	}
}