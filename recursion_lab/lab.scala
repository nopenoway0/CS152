object lab{
	def inc(n: Int) = n + 1
	def dec(n: Int) = n - 1
	
	def add(n: Int, m: Int): Int = if(n == 0) m else inc(add(dec(n), m))
	
	def mul(n: Int, m: Int): Int = if(n == 0) 0 else add(m, mul(dec(n), m))
	
	def exp2(m: Int): Int = if(m == 1) 2 else mul(2, exp2(dec(m)))

	def main(args:Array[String]) ={
		println(add(4, 5))
		println(mul(10, 5))
		println(mul(321, 10))
		println(exp2(4))
	}
}