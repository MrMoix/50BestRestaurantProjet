import com.github.tototoshi.csv.CSVReader

import java.io.File

object Restaurant {
  def main(args: Array[String]): Unit = {
    case class Restaurant(Ranking: String, Restaurant: String, City: String, Country: String, Lat: Float, Lon: Float, Stars: String, Chef: String, Website: String, Menu: Int, Currency: String, Description: String)


    val reader = CSVReader.open(new File("src/main/02-50BestRestaurants.csv"))
    val dataset = reader.toStream


    dataset.drop(1).foreach(t => for (test <- t) println(test.trim))
  }
}
