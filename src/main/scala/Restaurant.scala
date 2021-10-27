import com.github.tototoshi.csv.CSVReader

import java.io.File

object Restaurant extends App {

  trait Activity {
    val name: String
    val city: String
    val country: String
    val lat: Double
    val lon: Double

  }

  case class Restaurant(ranking: Int, name: String, city: String, country: String, lat: Double, lon: Double, stars: String, chef: String, website: String, menu: Double, currency: String, description: String) extends Activity {
    def intial(menu: Double) = menu

    def increase(menu: Double) = (menu + menu * 0.5).toDouble
  }


  val reader = CSVReader.open(new File("src/main/02-50BestRestaurants.csv"))
  val dataset = reader.toStream.toList.drop(1)

  val restaurants = dataset.map(resto => Restaurant(resto(0).toInt, resto(1).trim, resto(2), resto(3), resto(4).toDouble, resto(5).toDouble, resto(6), resto(7), resto(8), resto(9).toDouble, resto(10), resto(11)))

  val resto1Price = restaurants(0).intial(restaurants(0).menu)
  //println(resto1Price)
  //println(restaurants(0).increase(restaurants(0).menu))


  //println("Prix initial: " + resto1Price)

  //restaurants.foreach(println)
  //insertColumn.foreach(println)
  //println(restaurants)

  val test = restaurants.filter(x => restaurants(x.ranking - 1).ranking.toInt <= 10)
  //test.foreach(println)

  val test2 = restaurants.groupBy(x => x.country.contains("France")).map(x => x._2)
  //test2.foreach(println)

  val test3 = restaurants.sortBy(_.name).map(_.name)
  test3.foreach(println)

  val test4 = restaurants.filter(x => x.currency.contains("EUR")).map(x => x.name)

  //test4.foreach(println)

  val test5 = restaurants.filter(x => x.stars.contains("3")).filter(x => x.currency.contains("EUR")).map(x => x.menu).sum / restaurants.filter(x => x.stars.contains("3")).filter(x => x.currency.contains("EUR")).length


  print("Prix moyen du menu des restaurants 3 étoiles en EUR : "+test5)


}
