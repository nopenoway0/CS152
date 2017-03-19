 object dds{
	//def halt(currentState: S, cycle: Int) = if(currentState is final) true else false

	def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S = {
			if(halt(state, cycle)) state else
			controlLoop(update(state, cycle), cycle + 1, halt, update)
	}

	// Problem 6
	def double(pop: Int, week: Int) = 2 * pop
	def cc(pop: Int, week: Int) = 100000 < pop // check number

	//Problem 7
	val delta = 1e-5

	def deriv(f: Double=>Double) = {
		def df(x: Double) = f(x + delta) - f(x)/delta
		df _
	}

	def solve(f: Double => Double) = {
		val df = deriv(f)
		def goodEnuf(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
		def improve(guess: Double, cycle: Int) = guess - f(guess) / df(guess)
		controlLoop(1.0, 0, goodEnuf, improve)
	}

	// Problem 8
	def squareRoot(x: Double) = {
		def f(z: Double) = z*z - x
		solve(f)
	}

	//Problem 9
	def cubeRoot(x: Double) = {
		def f(z: Double) = z*z*z - x
		solve(f)
	}

	// Problem 10
	def nthRoot(x: Double, n: Int) = {
		def f(z: Double) = scala.math.pow(z, n) - x
		solve(f)
	}

	def main(args: Array[String]) = {
		println(nthRoot(27, 3))
		println("Square Root: " + squareRoot(121))
	}
}