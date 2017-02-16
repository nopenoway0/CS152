import Array._

object MatrixCalculator {
  
  // converts matrix to a string
  def toString(matrix: Array[Array[Int]]) = {
    var result: String = ""
    var row = 0
    var elem = 0
    for(row <- matrix){
      result = result + "\n"
      for(elem <-row) result = result + elem + " "
    }
    result
  }
  
  // returns the sum of the diagonal entries of a matrix
  def trace(m: Array[Array[Int]]) = {
    var x: Int = 0
    var sum: Int = 0
    for(x <- 0 to m.length - 1){
      sum += m(x)(x)
    }
    sum
  }
  
  // returns a dim x dim matrix with i/j entry = 3 * i + 2 * j % cap
  def makeArray(dim: Int, cap: Int = 100) = {
    var matrix: Array[Array[Int]] = Array.ofDim[Int](dim, dim)
    var x = 0
    var y = 0
    val bound = dim - 1
    var count = 10
    for(x <- 0 to bound)
      for(j <- 0 to bound){
        matrix(x)(j) = count
        count = count + 1
      }
    matrix
  }

  def main(args: Array[String]): Unit = {
    print("Enter a positive integer: ")
    var n = readInt
    var m = makeArray(n)
    println(toString(m))
    println("trace = " + trace(m))
  }

}