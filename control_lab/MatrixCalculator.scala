import Array._

object MatrixCalculator {
  
  // converts matrix to a string
  def toString(matrix: Array[Array[Int]]): String = {
    var result: String = ""
    var row = 0
    var elem = 0
    for(row <- matrix){
      result = result + "\n"
      for(elem <-row) result = result + elem + " "
    }
    return result
  }
  
  // returns the sum of the diagonal entries of a matrix
  def trace(m: Array[Array[Int]]): Int = {
    return 0
  }
  
  // returns a dim x dim matrix with i/j entry = 3 * i + 2 * j % cap
  def makeArray(dim: Int, cap: Int = 100): Array[Array[Int]] = {
    var matrix = Array.ofDim[Int](dim, dim)
    var x = 0
    var y = 0
    val bound = dim - 1
    var count = 10
    for(x <- 0 to bound)
      for(j <- 0 to bound){
        matrix(x)(j) = count
        count = count + 1
      }
    return matrix
  }

  def main(args: Array[String]): Unit = {
    print("Enter a positive integer: ")
    var n = readInt
    var m = makeArray(n)
    println(toString(m))
    println("trace = " + trace(m))
  }

}