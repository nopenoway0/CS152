object session1{
	//classical recursion
	def fact1(n: Int): Int = if(n == 1) 1 else n * fact1(n - 1)

	// iterative
	def fact2(n: Int) = {
		var fact = 1
		for (x <- 2 to n) fact = x * fact
		fact 
	}

	// tail
	def fact3(n: Int) = {
		def help(n: Int, prod: Int): Int = {
			if( n == 1 ) prod
			else help(n - 1, n * prod)
		}
		help(n, 1)
	}

	def main(args: Array[String]) = {
	println(fact1(3))
	println(fact2(3))
	println(fact3(3))
	}
}