object VectorCalculator {
  
  def add(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    var sum = v1.zip(v2).map {case (a,b) => a + b}
    return sum
  }
  
  def dot(v1: Array[Int], v2: Array[Int]): Int = {
    var x = 0
    var y = 0
    var dot: Int = 0
    v1.zip(v2).map {case (a,b) => dot = dot + a*b}
    return dot
  }
  
  def toString(v: Array[Int]) = {
    var result = "["
    for(e <- v) {
      result = result + " " + e
    }
    result = result + "]"
    result
  }

  def main(args: Array[String]): Unit = {
    try {
      print("Enter 3 integers: ")
      var x = readInt()
      var y = readInt()
      var z = readInt()
      val vec1 = Array(x, y, z)
      val vec2 = Array(1, 2, 3)
      val vec3 = add(vec1, vec2)
      println("sum = " + toString(vec3))
      println("dot = " + dot(vec1, vec2))
    } catch {
         case e: Exception => {println(e)}
    }
    
  }

}
