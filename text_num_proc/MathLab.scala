// Problems 1, 2, 3, 6, 7, 8
// 
// 

import scala.math._
import scala.util.Random

object MathLab{

	def main(args: Array[String]): Unit = {
		println("\nSolve Tests")
		println("solve(2, -2, -4): " + solve(2, -2, -4))
		println("solve(1, 0, 1): " + solve(1, 0, 1))
		println("solve(1, 0, -1): " + solve(1, 0, -1))

		println("\nDist Tests")
		println("Dist((1,1), (0,0)): " + dist((1, 1), (0, 0)))
		println("Dist((3,0), (0,0)): " + dist((3, 0), (0, 0)))
		
		println("\nDots Tests")
		println("dot((2.0, 3, 4), (2, 2.0, 2))" + dot((2.0, 3, 4), (2, 2.0, 2)))

		println("\nPrime Tests")
		println("isPrime(0): " +  isPrime(0))
		println("isPrime(1): " + isPrime(1))
		println("isPrime(2): " + isPrime(2))
		println("isPrime(3): " + isPrime(3))
		println("isPrime(8): " + isPrime(8))

		println("\nPhi Tests")
		println("Phi(9): " + phi(9))

		println("\nDice Tests")
		println(rollDice)
		println(rollDice)
		println(rollDice)
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

	//***********************************
	// Problem 7
	//***********************************
	def phi(in: Int) = {

		// Helper to find gcd of number
		def gcd(n1: Int, n2: Int) = {
			var gcd = 1
			for(x <- 2 until n1) if(n1 % x == 0 && n2 % x == 0 && x > gcd) gcd = x
			gcd
		}


		var count = 0
		for(x <- 2 until in){
			if(gcd(x, in) == 1) count += 1
		}
		count
	}
	//***********************************

	//***********************************
	// Problem 8
	//***********************************
	def rollDice = {
		val seed = new Random
		(seed.nextInt(6), seed.nextInt(6))
	}
	//***********************************


}