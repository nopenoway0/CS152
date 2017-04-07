class Dragon(private val _name:String = "Dragon"){
	var health: Int = 100

	def name = _name

	def attack(victim: Knight) = {
		victim.health = victim.health - Dungeon.random.nextInt(this.health)
		if(victim.health <= 0) victim.health = 0
		this.name + " is flaming " + victim.name
	}
	override def toString = this.name + " " + this.health
}