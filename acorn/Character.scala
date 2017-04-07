package character

class Character(health: Int = 100, name: String = "Char"){
	override def toString = this.name + " " + this.health
}