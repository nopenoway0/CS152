// Problems 1, 2, 3, 6, 7, 8
// 
// 

import scala.math._

object MathLab{

	def main(args: Array[String]): Unit = {
		println(solve(1, 0, 1))
		print(dist((1,3),(1,3)))
	}

	//***********************************
	// Problem 1
	//***********************************
	def solve(a: Int, b: Int, c: Int) = {
		val desc = pow(b, 2) - 4*a*c
		if(desc < 0) None
		else Some(((-1 * b) + sqrt(desc) / (2 * a), ((-1 * b) - sqrt(desc) / (2 * a))))
	}
	//***********************************

	def dist(x1: Int, y1: Int, x2: Int, y2: Int) = {
		println("holding")
	}
}