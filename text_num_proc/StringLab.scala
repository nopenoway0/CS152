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

	def main(args:Array[String]) = {
		println(isPal("rotator"))
		println(isPal("cat"))

		println(isPal2("A man, a plan, a canal, ..Panama?"))
		println(mkPal("mars"))
		println(mkPal("3X@#"))
	}
}