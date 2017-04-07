package value
import expression._


trait Value

class Number(val value: Double) extends Literal { 
  def +(other: Number) = Number(this.value + other.value)
  def *(other: Number) = Number(this.value * other.value)
  def -(other: Number) = Number(this.value - other.value)
  def /(other: Number) = Number(this.value / other.value)
  //def <(other: Number) = Boole(this.value < other.value)
  //def ==(other: Number) = Boole(this.value == other.value)
  override def toString  = value.toString
}

object Number {
  	def apply(value: Double) = new Number(value)
}