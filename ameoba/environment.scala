
package amoeba
import scala.collection.mutable.HashMap

class Environment(var extension: Environment = null) extends HashMap[String, Variable] {
  
  put("return", new Variable(0))
  
  override def apply(name: String) = {
    if (contains(name)) super.apply(name)
    else if (extension != null) extension(name)
    else throw new Exception("undefined variable: " + name)
  }
}