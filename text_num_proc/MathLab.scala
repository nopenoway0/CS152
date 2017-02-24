// Problems 1, 2, 3, 6, 7, 8
// 
// 

import scala.math._

object MathLab{

	def main(args: Array[String]): Unit = {
		println(solve(2, -2, -4))
		println(solve(1, 0, 1))
		println(solve(1, 0, -1))
		println(dist((1, 1), (0, 0)))
		println(dist((3, 0), (0, 0)))
		println(dot((2.0, 3, 4), (2, 2.0, 2)))

		println(isPrime(0))
		println(isPrime(1))
		println(isPrime(2))
		println(isPrime(3))
		println(isPrime(8))
	}

	//***********************************
	// Problem 1
	//***********************************
	def solve(a: Double, b: Double, c: Double) = {
		val desc = pow(b, 2) - 4*a*c
		if(desc < 0) None
		else Some((((-1 * b) + sqrt(desc)) / (2 * a), (((-1 * b) - sqrt(desc)) / (2 * a))))
	}
	//***********************************

	//***********************************
	// Problem 2
	//***********************************
	def dist(p1: (Double, Double), p2: (Double, Double)) = {
		val x_diff = (p1._1 - p2._1)
		val y_diff = (p1._2 - p2._2)
		sqrt(pow(x_diff, 2) + pow(y_diff, 2))
	}
	//***********************************

	//***********************************
	// Problem 3
	//***********************************
	def dot(v1:(Double, Double, Double), v2:(Double, Double, Double)) = v1._1*v2._1 + v1._2*v2._2 + v1._3*v2._3
	//***********************************

	//***********************************
	// Problem 6
	//***********************************
	def isPrime(in: Int) = {
		if(in < 0) throw new Exception("Negative Number")
		var result = true
		if(in == 0 || in == 1) result = false
		for( x <- 2 until in){
			if(in % x == 0) result = false
		}
		result
	}
	//***********************************

}