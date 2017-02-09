object ShapeMaker {
  
  def makeRectangle(rows: Int, cols: Int) = {
    var x = 0
    var y = 0
    for(x <- 1 to rows){
      for(y <- 1 to cols) print("*")
      println()
    }
  }
   
  def makeRightTriangle(rows: Int) = {
    // place implementation here
  }

  def makeIsoTriangle(rows: Int) = {
    // place implementation here
  }
   
  def makeInvertedTriangle(rows: Int) = {
    // place implementation here
  }

  def main(args: Array[String]): Unit = {
    print("Enter a positive integer: ")
    var n = readInt
    println(makeRectangle(n, n))
    println(makeRightTriangle(n))
    println(makeIsoTriangle(n))
    println(makeInvertedTriangle(n))
  }

}