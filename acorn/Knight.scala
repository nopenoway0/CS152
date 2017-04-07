class Knight(private val _name:String = "Knight"){
	var health: Int = 100

	def name = _name

	def attack(victim: Dragon) = {
		victim.health = victim.health - Dungeon.random.nextInt(this.health)
		if(victim.health <= 0) victim.health = 0
		this.name + " is stabbing " + victim.name
	}
	override def toString = this.name + " " + this.health
}