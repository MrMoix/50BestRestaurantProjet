import com.github.tototoshi.csv.CSVReader

import java.io.File

object Restaurant {
  def main(args: Array[String]): Unit = {
    case class Restaurant(Ranking: Int, Restaurant: String, City: String, Country: String, Lat: Double, Lon: Double, Stars: String, Chef: String, Website: String, Menu: Double, Currency: String, Description: String)


    val reader = CSVReader.open(new File("src/main/02-50BestRestaurants.csv"))
    val dataset = reader.toStream.toList.drop(1)

    val restaurants = dataset.map(resto => Restaurant(resto(0).toInt, resto(1).trim, resto(2), resto(3), resto(4).toDouble, resto(5).toDouble, resto(6), resto(7), resto(8), resto(9).toDouble, resto(10), resto(11)))

    //insertColumn.foreach(println)
    //println(restaurants)

    restaurants.filter(x=>restaurants(x.Ranking-1).Ranking.toInt<=10).foreach(println)
  }
}
