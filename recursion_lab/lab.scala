object lab{
	def inc(n: Int) = n + 1
	def dec(n: Int) = n - 1
	
	def add(n: Int, m: Int): Int = if(n == 0) m else inc(add(dec(n), m))
	
	def mul(n: Int, m: Int): Int = if(n == 0) 0 else add(m, mul(dec(n), m))
	
	def exp2(m: Int): Int = if(m == 1) 2 else mul(2, exp2(dec(m)))

	def hyperExp(m: Int): Int = if(m == 0) 1 else exp2(hyperExp(dec(m)))

	def tailAdd(n: Int, m: Int) = {
		def helper(count: Int, n: Int):Int = {
			if(count == 0) n else helper(dec(count), inc(n))
		}
		helper(n, m)
	}

	def tailMul(n: Int, m: Int) = {
		def helper(n: Int, m: Int): Int = {
			if(n == 1) m else tailAdd(m, helper(dec(n),m))
		}
		helper(n,m)
	}

	def main(args:Array[String]) ={
		println(add(4, 5))
		println(mul(10, 5))
		println(mul(321, 10))
		println(exp2(4))
		//println(hyperExp(4))
		println(tailAdd(21, 10))
		println(tailMul(10, 25))
	}
}