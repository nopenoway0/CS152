class Test(val in: Int){
	def +(other: Test) = new Test(this.in + other.in)
	override def toString = this.in.toString
}

object Test{
	def apply(in: Int) = new Test(in)
}

object Runner extends App{
	println("Test")
	val t = Test(1)
	println(t)
	println(t + t)
}