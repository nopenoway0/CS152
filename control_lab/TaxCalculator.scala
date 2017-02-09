object TaxCalculator {
  
  def tax(income: Double): Double = {
  		if(income < 0){
  			println("Income can't be negative")
  			throw new Exception
  		}
  		var rate = 0.0
  		if(income < 30000) rate = 0.05
  		else if(income < 40000) rate = 0.11
  		else if(income < 60000) rate = 0.23
  		else if(income < 100000) rate = 0.32
  		else rate = 0.50
  		return income * rate
   } 

}