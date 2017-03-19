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

	def selfIter[T](f: T=>T, n: Int): T=>T = if(n == 1) f else compose(f, selfIter(f, n - 1)) 

	// Problem 3
	def countPass[T](in: Array[T], f:T=>Boolean) = {
		var count = 0
		for(x <- in) if(f(x)) count += 1
		count
	}
	// Test functions p1
	def len(x: String) = x.length
	def isEven(x: Int) = x % 2 == 0

	// Test functions p2
	def doubleSelf(x: Int) = 2 * x

	// Test functions p2
	//def in(x: Double) = x + 1
	//def double(x: Double) = x * 2

	// test function p3
	def isOdd(in: Int) = if(in % 2 == 0) false else true

	def main(args:Array[String]) = {
		val hasEven = compose(isEven _, len _)
		println(hasEven("Bating"))

		val double = selfIter(doubleSelf _, 4) // should double 5 times
		println(double(2))

		val a = Array(1, 2, 3, 4, 5)
		println(countPass(a, isOdd _))
	}
}