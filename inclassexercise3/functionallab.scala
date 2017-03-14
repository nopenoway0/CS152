// Problems 1 - 3 and 5 - 10 
// Scala project called functions add worksheet caled functions
object functions2{
	def compose[T,S,U](x: T=>U, y: S=>T) = {
		def r(in: S) = x(y(in))
		r _
	}
	// Test functions
	def len(x: String) = x.length
	def isEven(x: Int) = x % 2 == 0

	def main(args:Array[String]) = {
		val hasEven = compose(isEven _, len _)
		println(hasEven("Bati"))
	}
}