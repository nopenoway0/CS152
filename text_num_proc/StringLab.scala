import scala.util.Random

object StringLab{
	def isPal(in:String) = {
		var tmp = in.trim
		var result = true
		for(x <- 0 until tmp.length / 2){
			if(tmp(x) != tmp(tmp.length - (x + 1))) result = false
		}
		result
	}

	def isPal2(in: String) = {
		var tmp = in.replaceAll("\\s|!|,|\\?|\\.", "").toLowerCase
		var result = true
		for(x <- 0 until tmp.length / 2){
			if(tmp(x) != tmp(tmp.length - (x + 1))) result = false
		}
		result
	}

	def mkPal(in: String) = {
		if(isPal2(in)) in
		else{
			var pal = in
			val length = pal.length
			for(x <- 1 to length) pal += pal(length - x)
			pal
		}
	}

	def mkWord(in: Int = 11) = {
		val seed = new Random
		var word = ""
		for(x <- 0 until in){
			word += (seed.nextInt(25) + 97).toChar
		}
		word
	}

	def mkSentence(sentence_length: Int = 6) = {
		val seed = new Random
		var sentence = ""
		for(x <- 0 until sentence_length){
			if(x > 0) sentence += " "
			sentence += mkWord(seed.nextInt(12) + 1)
		}
		sentence.replace(sentence(0).toString, sentence(0).toString.toUpperCase) + "."

	}

	def main(args:Array[String]) = {
		println("\nisPal Tests")
		println("rotator:" + isPal("rotator"))
		println("cat: " + isPal("cat"))

		println("\nisPal2 Tests")
		println("A man, a plan, a canal, ..Panama?: " + isPal2("A man, a plan, a canal, ..Panama?"))
		println("In girum imus nocte et consumimur igni: " + isPal2("In girum imus nocte et consumimur igni"))
		println("\nmkPal Tests")
		println("mars: " + mkPal("mars"))
		println("3X@#: " + mkPal("3X@#"))
		
		println("\nmkWord Tests")
		println(mkWord())
		println(mkWord())
		println(mkWord())
		println(mkWord(20))

		println("\nMK Sentence Tests")
		println(mkSentence())
		println(mkSentence())
		println(mkSentence())
		println(mkSentence())
		println(mkSentence(5))

	}
}