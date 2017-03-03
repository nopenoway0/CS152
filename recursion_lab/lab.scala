object lab{
	def inc(n: Int) = n + 1
	def dec(n: Int) = n - 1
	def isZero(n: Int) = n == 0

	def add(n: Int, m: Int): Int = if(isZero(n)) m else inc(add(dec(n), m))
	
	def mul(n: Int, m: Int): Int = if(isZero(n)) 0 else add(m, mul(dec(n), m))
	
	def exp2(m: Int): Int = if(m == 1) 2 else mul(2, exp2(dec(m)))

	def hyperExp(m: Int): Int = if(isZero(m)) 1 else exp2(hyperExp(dec(m)))

	def tailAdd(n: Int, m: Int) = {
		def helper(count: Int, n: Int):Int = {
			if(isZero(count)) n else helper(dec(count), inc(n))
		}
		helper(n, m)
	}

	def tailMul(n: Int, m: Int) = {
		def helper(n: Int, m: Int): Int = {
			if(n == 1) m else tailAdd(m, helper(dec(n),m))
		}
		helper(n,m)
	}

	def tailExp2(m: Int) = {
		def helper(m: Int): Int = {
			if(m == 1) 2 else if(isZero(m)) 1 else tailMul(2, helper(dec(m)))
		}
		helper(m)
	}

	def tailHyperExp(m: Int) = {
		def helper(count : Int, result: Int): Int = {
			if( count == 1) result else helper(dec(count), tailExp2(result))
		}
		helper(m, 2)
	}

	def fib(n: Int): Int = {
		if(n == 0) 0
		else if (n == 1) 1
		else fib(dec(n)) + fib(dec(dec(n)))
	}

	def tailFib(n: Int) = {
		def helper(n: Int, m: Int, result: Int): Int = {
			if(isZero(n)) 0 else if (n == 1) 1 else helper(dec(n), m, m + n)
		}
		helper(n, n, 0)
	}

	def choose(n: Int, m: Int) = {
		def helper(n: Int, m: Int): Int = {
			if( n < m ) 0
			else if ( n == m ) 1
			else if ( m == 0 ) 1
			helper(dec(n), dec(m))
		}
		helper(n, m)
	}

	def main(args:Array[String]) ={
		println(add(4, 5))
		println(mul(10, 5))
		println(mul(321, 10))
		println(exp2(4))
		//println(hyperExp(4))
		println(tailAdd(21, 10))
		println(tailMul(10, 25))
		println(tailExp2(4))
		println(tailExp2(0))
		println(fib(5))
		println(tailFib(5))
		//println(choose(4,2)) // = 6
	}
}