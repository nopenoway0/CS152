package amoeba

class Variable(var cont: Int = 0) {
	var content:Int = cont

	override def toString = content.toString
}