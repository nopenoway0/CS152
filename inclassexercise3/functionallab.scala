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

	def selfIter[T](f: T=>T, n: Int): T=>T = {
		//def fun(in: T) = f(in)
		if(n == 1) f else compose(f, selfIter(f, n - 1)) 
		//compose(f, f)
	}

	// Problem 3
	


	// Test functions p1
	def len(x: String) = x.length
	def isEven(x: Int) = x % 2 == 0

	// Test functions p2
	def addSelf(x: Int) = 2 * x

	// Test functions p2
	//def in(x: Double) = x + 1
	//def double(x: Double) = x * 2

	def main(args:Array[String]) = {
		val hasEven = compose(isEven _, len _)
		println(hasEven("Bating"))

		val double = selfIter(addSelf _, 1)
		println(double(2))
	}
}