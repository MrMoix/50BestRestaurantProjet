import com.github.tototoshi.csv.CSVReader

import java.io.File

object Restaurant extends App {

  case class Restaurant(Ranking: Int, Restaurant: String, City: String, Country: String, Lat: Double, Lon: Double, Stars: String, Chef: String, Website: String, Menu:Double, Currency: String, Description: String) {
    def intial(Menu: Double) = Menu

    def increase(Menu: Double) = (Menu + Menu * 0.5).toDouble
  }


  val reader = CSVReader.open(new File("src/main/02-50BestRestaurants.csv"))
  val dataset = reader.toStream.toList.drop(1)

  val restaurants = dataset.map(resto => Restaurant(resto(0).toInt, resto(1).trim, resto(2), resto(3), resto(4).toDouble, resto(5).toDouble, resto(6), resto(7), resto(8), resto(9).toDouble, resto(10), resto(11)))

  //val resto1Price = restaurants(0).intial(restaurants(0).Menu)
  println(restaurants(0).increase(restaurants(0).Menu))
  


  //println("Prix initial: " + resto1Price)

  //restaurants.foreach(println)
  //insertColumn.foreach(println)
  //println(restaurants)

  val test = restaurants.filter(x => restaurants(x.Ranking - 1).Ranking.toInt <= 10)
  //test.foreach(println)

  val test2 = restaurants.groupBy(x => x.Country.contains("France")).map(x => x)
  //test2.foreach(println)

  val test3 = restaurants.sortBy(_.Restaurant).groupMap(x => x.Menu)(x => x.Restaurant)
  // test3.foreach(println)

  val test4 = restaurants.filter(x => x.Currency.contains("EUR")).map(x => x.Restaurant)

  //test4.foreach(println)

  val test5 = restaurants.filter(x => x.Stars.contains("3")).map(x => x.Menu).sum / restaurants.length


  //print(test5)


}
