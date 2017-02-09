object Calculator {

   // = 1 * 2 * 3 * ... * n
   def fact(n: Integer): Int = {
    var base_fact = n
    var sum = 1
    while(base_fact > 1){
      sum = sum * base_fact
      base_fact = base_fact - 1
    }
    return sum
    }
   
   // = 1 + 2 + 3 + ... + n
   def tri(n: Integer): Int = {
    if(n > 1){
      return n + tri(n - 1)
    }
    else return 1
   }
   
   // = 2^n
   def exp(n: Integer): Int = {
      val base = 2
      var product = 1
      var x = 0
      for(a <- 1 to n) product = product * base
      return product
    }
   
   // = true if n >= 2 and has no smaller divisors
   def isPrime(n: Integer): Boolean = {
    var x = 0
    if(n == 1) return false
    if(n == 2 || n == 3) return true
    for(x <- 2 to n - 1){
      if(n % x == 0) return false
    }
    return true
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
