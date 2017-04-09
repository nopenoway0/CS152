package temperature

trait IThermometer{
	// = avg degrees Farenheit
	def getMeanTemperature(cities: List[String]): Double = {
		9.0
	}
}

object CenTherm extends IThermometer{
	def computeTemp(city: String): Double = {
		super.getMeanTemperature(List(city)) * 9 / 5 + 32
	}
}