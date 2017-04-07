object Dungeon{
	val random = new scala.util.Random(System.nanoTime)
	def main(args:Array[String]) = {
		val d1 = new Dragon
		val k1 = new Knight
		var done = false
		while(!done){
			println(k1.attack(d1))
			if(d1.health <= 0) done = true
			else println(d1.attack(k1))
		}
		println(d1.name + " " + d1.health)
		println(k1.name + " " + k1.health)
	}	
}