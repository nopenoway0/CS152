// Problems 1 - 3 and 5 - 10 
// Scala project called functions add worksheet caled functions
object functions2{
	// Problem 1
	def compose[T,S,U](x: T=>U, y: S=>T) = {
		def r(in: S) = x(y(in))
		r _
	}

	// Problem 2
	def id[T](x: T) = x

	def selfIter[T, S](f: T=>S, n: Int): T=>T = {
		if(n == 0) id _ else compose(f _, selfIter(f, n - 1))
	}

	// Problem 3
	
	// Test functions p1
	def len(x: String) = x.length
	def isEven(x: Int) = x % 2 == 0

	// Test functions p2
	//def in(x: Double) = x + 1
	//def double(x: Double) = x * 2

	def main(args:Array[String]) = {
		val hasEven = compose(isEven _, len _)
		println(hasEven("Bating"))
	}
}