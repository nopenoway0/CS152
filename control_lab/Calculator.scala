object Calculator {

   // = 1 * 2 * 3 * ... * n
   def fact(n: Integer) = {
    var base_fact = n
    var sum = 1
    while(base_fact > 1){
      sum = sum * base_fact
      base_fact = base_fact - 1
    }
    sum
    }
   
   // = 1 + 2 + 3 + ... + n
   def tri(n: Integer) = {
    var sum = 0
    var x  = 0
    for(x <- 0 to n) sum += x
    sum
   }
   
   // = 2^n
   def exp(n: Integer) = {
      val base = 2
      var product = 1
      var x = 0
      for(a <- 1 to n) product = product * base
      product
    }
   
   // = true if n >= 2 and has no smaller dcivisors
   def isPrime(n: Integer) = {
    var x = 0
    var result = if(n == 1 || n == 0) false else true
    for(x <- 2 until n){
      if(n % x == 0 && n != 0) result = false
    }
    result
   }
   
   def main(args: Array[String]): Unit = {
     println("enter 3 integers x, y, and z on separate lines: ")
     var x = readInt()
     var y = readInt()
     var z = readInt()
     println("fact(x) = " + fact(x))
     println("fact(y) = " + fact(y))
     println("fact(z) = " + fact(z))
     println("tri(x) = " + tri(x))
     println("tri(y) = " + tri(y))
     println("tri(z) = " + tri(z))
     println("exp(x) = " + exp(x))
     println("exp(y) = " + exp(y))
     println("exp(z) = " + exp(z))
     println("isPrime(x) = " + isPrime(x))
     println("isPrime(y) = " + isPrime(y))
     println("isPrime(z) = " + isPrime(z))
   }

}
