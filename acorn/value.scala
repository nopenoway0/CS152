package value
import expression._


trait Value

class Number(val value: Double) extends Literal { 
  def +(other: Number) = Number(this.value + other.value)
  def *(other: Number) = Number(this.value * other.value)
  def -(other: Number) = Number(this.value - other.value)
  def /(other: Number) = Number(this.value / other.value)
  def <(other: Number) = Boole(this.value < other.value)
  def ==(other: Number) = Boole(this.value == other.value)
  override def toString  = value.toString
}

object Number {
  	def apply(value: Double) = new Number(value)
}

class Boole(val value: Boolean) extends Literal{
	def &&(other: Boole) = Boole(this.value && other.value)
	override def toString = this.value.toString
}

object Boole{
	def apply(value: Boolean) = new Boole(value)
	val TRUE = Boole(true)
	val FALSE = Boole(false)
}